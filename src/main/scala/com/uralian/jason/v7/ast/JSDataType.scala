package com.uralian.jason.v7.ast

import com.uralian.jason.util.JsonUtils
import org.json4s.FieldSerializer.renameFrom
import org.json4s.JsonAST.JArray
import org.json4s.{FieldSerializer, JObject, JValue}

import java.util.regex.Pattern

/**
 * JSON Schema type annotation.
 *
 * @param title       type name.
 * @param description type description.
 * @param default     default value.
 * @param examples    example values.
 * @param readOnly    whether the value can be only read (eg. in an API request).
 * @param writeOnly   whether the value can be only written (eg. in an API request).
 */
final case class Annotation private(title: Option[String] = None,
                                    description: Option[String] = None,
                                    default: Option[JValue] = None,
                                    examples: Option[JArray] = None,
                                    comment: Option[String] = None,
                                    readOnly: Option[Boolean] = None,
                                    writeOnly: Option[Boolean] = None)

/**
 * Factory for [[Annotation]] instances.
 */
object Annotation extends JsonUtils {
  /**
   * JSON serializer for [[Annotation]] instances.
   */
  val serializer = FieldSerializer[Annotation](
    deserializer = renameFrom("$comment", "comment")
  )

  /**
   * Checks if the [[Annotation]] instance contains any information.
   *
   * @param annotation annotation to check.
   * @return `Some(annotation)` if the annotation is not empty, `None` otherwise.
   */
  def noneIfEmpty(annotation: Option[Annotation]): Option[Annotation] = {
    annotation.filter(a =>
      a.title.isDefined || a.description.isDefined || a.comment.isDefined || a.examples.isDefined ||
        a.default.isDefined || a.readOnly.isDefined || a.writeOnly.isDefined
    )
  }
}

/**
 * JSON Schema data type.
 */
sealed trait JSDataType {
  /**
   * JSON Schema type name.
   *
   * @return JS type name.
   */
  def typeName: JSTypeName

  /**
   * JSON Schema type annotation.
   *
   * @return optional type annotation.
   */
  def annotation: Option[Annotation]
}

/**
 * Primitive types: strings, numbers, booleans and null.
 */
sealed trait JSPrimitiveType extends JSDataType

/**
 * JSON Schema String data type.
 *
 * @param annotation type annotation.
 * @param minLength  minimum string length.
 * @param maxLength  maximum string length.
 * @param pattern    regex pattern for valid strings.
 * @param format     string format.
 */
final case class JSString private(annotation: Option[Annotation] = None,
                                  minLength: Option[Int] = None,
                                  maxLength: Option[Int] = None,
                                  pattern: Option[Pattern] = None,
                                  format: Option[JSStringFormat] = None) extends JSPrimitiveType {
  assert(minLength.forall(_ >= 0))
  assert(maxLength.forall(_ >= 0))

  val typeName: JSTypeName = JSTypeName.JSString
}

/**
 * Factory for [[JSString]] instances.
 */
object JSString extends JsonUtils {

  /**
   * JSON serializer for [[JSString]] instances.
   */
  val serializer = deserializer[JSString](_ => {
    case jv: JObject =>
      JSString(
        annotation = Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        minLength = extractJson[Option[Int]](jv \ "minLength"),
        maxLength = extractJson[Option[Int]](jv \ "maxLength"),
        pattern = extractJson[Option[Pattern]](jv \ "pattern"),
        format = extractJson[Option[JSStringFormat]](jv \ "format"))
  })
}

/**
 * JSON Schema Integer data type.
 *
 * @param annotation       type annotation.
 * @param multipleOf       restricts numbers to a multiple of.
 * @param minimum          inclusive minimum.
 * @param exclusiveMinimum exclusive minimum.
 * @param maximum          inclusive maximum.
 * @param exclusiveMaximum exclusive maximum.
 */
final case class JSInteger private(annotation: Option[Annotation] = None,
                                   multipleOf: Option[BigDecimal] = None,
                                   minimum: Option[BigDecimal] = None,
                                   exclusiveMinimum: Option[BigDecimal] = None,
                                   maximum: Option[BigDecimal] = None,
                                   exclusiveMaximum: Option[BigDecimal] = None) extends JSPrimitiveType {
  assert(multipleOf.forall(_ > 0))

  val typeName: JSTypeName = JSTypeName.JSInteger
}

/**
 * Factory for [[JSInteger]] instances.
 */
object JSInteger extends JsonUtils {

  /**
   * JSON serializer for [[JSInteger]] instances.
   */
  val serializer = deserializer[JSInteger](_ => {
    case jv: JObject =>
      JSInteger(
        annotation = Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        multipleOf = extractJson[Option[BigDecimal]](jv \ "multipleOf"),
        minimum = extractJson[Option[BigDecimal]](jv \ "minimum"),
        exclusiveMinimum = extractJson[Option[BigDecimal]](jv \ "exclusiveMinimum"),
        maximum = extractJson[Option[BigDecimal]](jv \ "maximum"),
        exclusiveMaximum = extractJson[Option[BigDecimal]](jv \ "exclusiveMaximum"))
  })
}

/**
 * JSON Schema Number data type.
 *
 * @param annotation       type annotation.
 * @param multipleOf       restricts numbers to a multiple of.
 * @param minimum          inclusive minimum.
 * @param exclusiveMinimum exclusive minimum.
 * @param maximum          inclusive maximum.
 * @param exclusiveMaximum exclusive maximum.
 */
final case class JSNumber private(annotation: Option[Annotation] = None,
                                  multipleOf: Option[BigDecimal] = None,
                                  minimum: Option[BigDecimal] = None,
                                  exclusiveMinimum: Option[BigDecimal] = None,
                                  maximum: Option[BigDecimal] = None,
                                  exclusiveMaximum: Option[BigDecimal] = None) extends JSPrimitiveType {
  assert(multipleOf.forall(_ > 0))

  val typeName: JSTypeName = JSTypeName.JSNumber
}


/**
 * Factory for [[JSNumber]] instances.
 */
object JSNumber extends JsonUtils {

  /**
   * JSON serializer for [[JSNumber]] instances.
   */
  val serializer = deserializer[JSNumber](_ => {
    case jv: JObject =>
      JSNumber(
        annotation = Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        multipleOf = extractJson[Option[BigDecimal]](jv \ "multipleOf"),
        minimum = extractJson[Option[BigDecimal]](jv \ "minimum"),
        exclusiveMinimum = extractJson[Option[BigDecimal]](jv \ "exclusiveMinimum"),
        maximum = extractJson[Option[BigDecimal]](jv \ "maximum"),
        exclusiveMaximum = extractJson[Option[BigDecimal]](jv \ "exclusiveMaximum"))
  })
}

/**
 * JSON Schema Boolean data type.
 *
 * @param annotation type annotation.
 */
final case class JSBoolean private(annotation: Option[Annotation] = None) extends JSPrimitiveType {
  val typeName: JSTypeName = JSTypeName.JSBoolean
}

/**
 * Factory for [[JSBoolean]] instances.
 */
object JSBoolean extends JsonUtils {

  /**
   * JSON serializer for [[JSBoolean]] instances.
   */
  val serializer = deserializer[JSBoolean](_ => {
    case jv: JObject =>
      JSBoolean(Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)))
  })
}

/**
 * JSON Schema Null data type.
 *
 * @param annotation type annotation.
 */
final case class JSNull private(annotation: Option[Annotation] = None) extends JSPrimitiveType {
  val typeName: JSTypeName = JSTypeName.JSNull
}

/**
 * Factory for [[JSNull]] instances.
 */
object JSNull extends JsonUtils {

  /**
   * JSON serializer for [[JSNull]] instances.
   */
  val serializer = deserializer[JSNull](_ => {
    case jv: JObject =>
      JSNull(Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)))
  })
}
