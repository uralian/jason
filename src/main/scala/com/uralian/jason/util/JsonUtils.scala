package com.uralian.jason.util

import org.json4s._
import org.json4s.native.JsonMethods

import java.util.regex.Pattern
import scala.reflect.ClassTag

/**
 * Various routines for JSON manipulation.
 */
trait JsonUtils {

  /**
   * Parses the input and returns a JSON AST.
   *
   * @param input input.
   * @return JSON structure.
   */
  def parseJson(input: JsonInput): JValue = JsonMethods.parse(input)

  /**
   * Renders JValue as a string.
   *
   * @param jv      JSON AST.
   * @param compact whether to produce compact JSON string.
   * @return JSON string.
   */
  def renderJson(jv: JValue, compact: Boolean = true): String = {
    val json = JsonMethods.render(jv)
    if (compact) JsonMethods.compact(json) else JsonMethods.pretty(json)
  }

  /**
   * Converts a JSON AST into an instance of a class.
   *
   * @param jv      JSON AST.
   * @param formats conversion formats.
   * @param mf      target class manifest.
   * @tparam T target type.
   * @return an instance of the target type.
   */
  def extractJson[T](jv: JValue)(implicit formats: Formats, mf: Manifest[T]): T =
    adjust(jv).extract(formats, mf)


  /**
   * Converts an object into JSON AST.
   *
   * @param a       source object.
   * @param formats conversion formats.
   * @return JSON AST.
   */
  def decomposeJson(a: Any)(implicit formats: Formats): JValue = adjust(Extraction.decompose(a))

  /**
   * Parses input into JSON AST and converts it into an instance of the target type.
   * This is a shortcut for `extractJson(parseJson(input))`.
   *
   * @param input   input
   * @param formats conversion formats.
   * @param mf      target class manifest.
   * @tparam T target type.
   * @return an instance of the target type.
   */
  def readJson[T](input: JsonInput)(implicit formats: Formats, mf: Manifest[T]): T = {
    val json = JsonMethods.parse(input, formats.wantsBigDecimal, formats.wantsBigInt)
    extractJson(json)
  }

  /**
   * Parses input string into JSON AST and converts it into an instance of the target type.
   * This is a shortcut for `readJson(StringInput(str))`.
   *
   * @param str     input string.
   * @param formats conversion formats.
   * @param mf      target class manifest.
   * @tparam T target type.
   * @return an instance of the target type.
   */
  def readJson[T](str: String)(implicit formats: Formats, mf: Manifest[T]): T = readJson(StringInput(str))

  /**
   * Converts input into JSON AST and then renders it into a JSON string.
   * This is a shortcut for `JsonMethod.render(decomposeJson(a))`.
   *
   * @param a       source value.
   * @param formats conversion formats.
   * @return JSON string.
   */
  def writeJson(a: Any, compact: Boolean = true)(implicit formats: Formats): String = {
    val json = decomposeJson(a)
    if (compact)
      JsonMethods.compact(JsonMethods.render(json))
    else
      JsonMethods.pretty(JsonMethods.render(json))
  }

  private def adjust(jv: JValue)(implicit formats: Formats): JValue =
    formats.emptyValueStrategy.noneValReplacement.map(_ => jv).getOrElse(jv.noNulls)

  /* custom serializer helpers */

  /**
   * Creates a custom serializer that is used ONLY for deserializing JSON.
   * At attempt to serialize a value into JSON will result in [[NotImplementedError]].
   *
   * @param des function that take Formats and returns a deserialization function.
   * @tparam A the target value type.
   * @return custom JSON deserializer for type A.
   */
  def deserializer[A: ClassTag](des: Formats => PartialFunction[JValue, A]): CustomSerializer[A] = {
    val f = (fmt: Formats) => (des(fmt), new PartialFunction[Any, JValue] {
      def isDefinedAt(x: Any): Boolean = false

      // $COVERAGE-OFF$
      def apply(v1: Any): JValue = ???
      // $COVERAGE-ON$
    })
    new CustomSerializer[A](f)
  }

  def objectDeserializer[A: ClassTag](objToValue: JObject => A): CustomSerializer[A] = {
    val pf: PartialFunction[JValue, A] = {
      case obj: JObject => objToValue(obj)
    }
    deserializer(_ => pf)
  }


  /* common serializers */

  val patternSerializer = new CustomSerializer[Pattern](_ => ( {
    case JString(str) => Pattern.compile(str)
  }, {
    case p: Pattern => JString(p.pattern())
  }))

  val patternKeySerializer = new CustomKeySerializer[Pattern](_ => ( {
    case str => Pattern.compile(str)
  }, {
    case p: Pattern => p.pattern()
  }))
}

/**
 * Singleton object for JSON helpers.
 */
object JsonUtils extends JsonUtils {

  /**
   * JSON formats.
   */
  val formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction.withBigDecimal

  /**
   * Serializers for common types.
   */
  val commonSerializers = List(patternSerializer)
}