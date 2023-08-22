package com.uralian.jason

import org.scalactic.Requirements._

import java.lang.reflect.{InvocationTargetException, Method, Modifier}

/**
 * This is a fork of Scalatest [[org.scalatest.PrivateMethodTester]], which fails to
 * invoke the method when it has a type parameter. Eg:
 *
 * {{{
 *   sealed trait Base[T <: JValue] {
 *     def build(arg: T): String
 *   }
 *
 *   object Base {
 *     implicit object StringBase extends Base[JString] {
 *       def build(arg: JString) = arg.s
 *     }
 *   }
 * }}}
 *
 * A simple hack is to add a check that the method is non-volatile.
 */
@SuppressWarnings(Array(
  "org.wartremover.warts.Throw",
  "org.wartremover.warts.Null",
  "org.wartremover.warts.AsInstanceOf",
  "org.wartremover.warts.StringPlusAny"))
trait InternalMethodTester {

  final class InternalMethod[T] private(methodName: Symbol) {
    requireNonNull(methodName)

    def apply(args: Any*) = new Invocation[T](methodName, args: _*)
  }

  object InternalMethod {
    def apply[T](methodName: Symbol) = new InternalMethod[T](methodName)
  }

  final class Invocation[T](val methodName: Symbol, val args: Any*) {
    requireNonNull(methodName)
  }

  final class Invoker(target: AnyRef) {
    requireNonNull(target)

    def invokePrivate[T](invocation: Invocation[T]): T = {
      import invocation._

      val methodNameToInvoke = methodName.name

      def isMethodToInvoke(m: Method) = {

        val isInstanceMethod = !Modifier.isStatic(m.getModifiers())
        val simpleName = m.getName
        val paramTypes = m.getParameterTypes
        val nonVolatile = !Modifier.isVolatile(m.getModifiers)

        def argsHaveValidTypes: Boolean = {

          if (args.length == paramTypes.length) {
            val zipped = args.toList zip paramTypes.toList

            def argMatchesParamType(arg: Any, paramType: Class[_]) = {

              Option(arg.asInstanceOf[AnyRef]).fold(true) { anyRefArg =>
                paramType match {
                  case java.lang.Long.TYPE      => anyRefArg.getClass == classOf[java.lang.Long]
                  case java.lang.Integer.TYPE   => anyRefArg.getClass == classOf[java.lang.Integer]
                  case java.lang.Short.TYPE     => anyRefArg.getClass == classOf[java.lang.Short]
                  case java.lang.Byte.TYPE      => anyRefArg.getClass == classOf[java.lang.Byte]
                  case java.lang.Character.TYPE => anyRefArg.getClass == classOf[java.lang.Character]
                  case java.lang.Double.TYPE    => anyRefArg.getClass == classOf[java.lang.Double]
                  case java.lang.Float.TYPE     => anyRefArg.getClass == classOf[java.lang.Float]
                  case java.lang.Boolean.TYPE   => anyRefArg.getClass == classOf[java.lang.Boolean]
                  case _                        => paramType.isAssignableFrom(anyRefArg.getClass)
                }
              }
            }

            val invalidArgs =
              for ((arg, paramType) <- zipped if !argMatchesParamType(arg, paramType)) yield arg
            invalidArgs.length == 0
          }
          else false
        }

        isInstanceMethod &&
          (simpleName == methodNameToInvoke || simpleName.endsWith("$$" + methodNameToInvoke)) &&
          argsHaveValidTypes && nonVolatile
      }

      val methodArray = for {
        m <- target.getClass.getDeclaredMethods if isMethodToInvoke(m)
      } yield m

      if (methodArray.length == 0)
        throw new IllegalArgumentException("Can't find a private method named: " + methodNameToInvoke)
      else if (methodArray.length > 1)
        throw new IllegalArgumentException("Found two methods")
      else {
        val anyRefArgs = // Need to box these myself, because that's invoke is expecting an Array[Object], which maps to an Array[AnyRef]
          for (arg <- args) yield arg match {
            case any: Any => any.asInstanceOf[AnyRef] // Can't use AnyVal in 2.8
            case null     => null
          }
        val privateMethodToInvoke = methodArray(0)
        privateMethodToInvoke.setAccessible(true)
        try {
          privateMethodToInvoke.invoke(target, anyRefArgs.toArray: _*).asInstanceOf[T]
        }
        catch {
          case e: InvocationTargetException =>
            val cause = e.getCause
            if (cause != null) throw cause else throw e
        }
      }
    }
  }

  implicit def anyRefToInvoker(target: AnyRef): Invoker = new Invoker(target)
}

object InternalMethodTester extends InternalMethodTester