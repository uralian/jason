package com.uralian.jason.v7.ast

import com.uralian.jason.util.JsonUtils
import org.json4s.FieldSerializer.renameFrom
import org.json4s.{FieldSerializer, JArray, JBool, JNothing, JObject, JString, JValue}

import java.util.regex.Pattern

/**
 * JSON Schema data type.
 */
sealed trait JSDataType {

  /**
   * JSON Schema type annotation.
   *
   * @return optional type annotation.
   */
  def annotation: Option[Annotation]
}

/**
 * Factory for [[JSDataType]] instances.
 */
object JSDataType extends JsonUtils {

  private val valueBased: PartialFunction[JValue, JSDataType] = {
    case JBool(true)                                 => JSAnything
    case JBool(false)                                => JSNothing
    case obj: JObject if (obj \ "const") != JNothing => extractJson[JSConst](obj)
    case obj: JObject if (obj \ "enum") != JNothing  => extractJson[JSEnum](obj)
  }

  private val explicitTypeResolver = {
    def getType(obj: JObject) = obj \ "type" match {
      case JString(s) => Some(s)
      case _          => None
    }

    val extractType: PartialFunction[JValue, (JObject, Option[String])] = {
      case obj: JObject if getType(obj).isDefined => (obj, getType(obj))
    }

    val convert: PartialFunction[(JObject, Option[String]), JSDataType] = {
      case (obj: JObject, Some("string"))  => extractJson[JSString](obj)
      case (obj: JObject, Some("integer")) => extractJson[JSInteger](obj)
      case (obj: JObject, Some("number"))  => extractJson[JSNumber](obj)
      case (obj: JObject, Some("boolean")) => extractJson[JSBoolean](obj)
      case (obj: JObject, Some("null"))    => extractJson[JSNull](obj)
      case (obj: JObject, Some("array"))   => extractJson[JSArray](obj)
      case (obj: JObject, Some("object"))  => extractJson[JSObject](obj)
    }

    extractType andThen convert
  }

  private val implicitTypeResolver = {

    def resolveByFields(fields: Iterable[String],
                        func: JValue => JSDataType): PartialFunction[JValue, JSDataType] = {
      case jv if fields.map(jv \ _).exists(_ != JNothing) => func(jv)
    }

    val resolveString = resolveByFields(
      List("minLength", "maxLength", "pattern", "format", "contentMediaType", "contentEncoding"),
      extractJson[JSString]
    )

    val resolveNumber = resolveByFields(
      List("minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum", "multipleOf"),
      extractJson[JSNumber]
    )

    val resolveArray = resolveByFields(
      List("contains", "minItems", "maxItems", "unique"),
      extractJson[JSArray]
    )

    val resolveObject = resolveByFields(
      List("properties", "patternProperties", "additionalProperties", "required", "propertyNames",
        "minProperties", "maxProperties"),
      extractJson[JSObject]
    )

    resolveString orElse resolveNumber orElse resolveArray orElse resolveObject
  }

  private val des: PartialFunction[JValue, JSDataType] =
    valueBased orElse explicitTypeResolver orElse implicitTypeResolver orElse {
      case _ => JSAnything
    }

  /**
   * JSON serializer for [[JSDataType]] instances.
   */
  val serializer = deserializer[JSDataType](_ => des)
}

/**
 * Type that accepts any data.
 */
case object JSAnything extends JSDataType {
  val annotation: Option[Annotation] = None
}

/**
 * Type that accepts no data.
 */
case object JSNothing extends JSDataType {
  val annotation: Option[Annotation] = None
}

/**
 * Scalar types: strings, numbers, booleans and null, as well as enum and const.
 */
sealed trait JSScalarType extends JSDataType

/**
 * JSON Schema String data type.
 *
 * @param annotation type annotation.
 * @param minLength  minimum string length.
 * @param maxLength  maximum string length.
 * @param pattern    regex pattern for valid strings.
 * @param format     string format.
 * @param content    media content.
 */
final case class JSString private(annotation: Option[Annotation] = None,
                                  minLength: Option[Int] = None,
                                  maxLength: Option[Int] = None,
                                  pattern: Option[Pattern] = None,
                                  format: Option[JSStringFormat] = None,
                                  content: Option[MediaContent] = None) extends JSScalarType {
  assert(minLength.forall(_ >= 0))
  assert(maxLength.forall(_ >= 0))
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
        format = extractJson[Option[JSStringFormat]](jv \ "format"),
        content = MediaContent.noneIfEmpty(extractJson[Option[MediaContent]](jv)))
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
                                   exclusiveMaximum: Option[BigDecimal] = None) extends JSScalarType {
  assert(multipleOf.forall(_ > 0))
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
                                  exclusiveMaximum: Option[BigDecimal] = None) extends JSScalarType {
  assert(multipleOf.forall(_ > 0))
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
final case class JSBoolean private(annotation: Option[Annotation] = None) extends JSScalarType

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
final case class JSNull private(annotation: Option[Annotation] = None) extends JSScalarType

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

/**
 * Array schema: can be either a typed list or a tuple.
 */
sealed trait ArraySchema

/**
 * A typed list.
 *
 * @param itemType type of each list item.
 */
final case class ListType(itemType: JSDataType) extends ArraySchema

/**
 * Factory for [[ListType]] instances.
 */
object ListType {
  /**
   * JSON serializer for [[ListType]] instances.
   */
  val serializer = FieldSerializer[ListType](
    deserializer = renameFrom("items", "itemType")
  )
}

/**
 * Type of items in the tuple.
 *
 * @param itemTypes  types of items in the tuple.
 * @param allowExtra the type of the allowed extra items. JSON Schema supports either a boolean
 *                   to indicate whether they are allowed or a specific type of allowed items.
 *                   We capture this as as generalized [[JSDataType]], where:
 *                   - [[JSAnything]] means any items are allowed (`true` in JSON schema)
 *                   - [[JSNothing]] means no items are allowed (`false` in JSON schema)
 *                   - any other type defines which items are specifically allowed.
 */
final case class TupleType(itemTypes: List[JSDataType],
                           allowExtra: Option[JSDataType]) extends ArraySchema

/**
 * Factory for [[TupleType]] instances.
 */
object TupleType extends JsonUtils {
  /**
   * JSON serializer for [[TupleType]] instances.
   */
  val serializer = deserializer[TupleType](_ => {
    case jv: JObject =>
      val itemTypes = extractJson[List[JSDataType]](jv \ "items")
      val allowExtra = jv \ "additionalItems" match {
        case JBool(true)  => Some(JSAnything)
        case JBool(false) => Some(JSNothing)
        case obj: JObject => Some(extractJson[JSDataType](obj))
        case _            => None
      }
      TupleType(itemTypes = itemTypes, allowExtra = allowExtra)
  })
}

/**
 * JSON schema Array data type.
 *
 * @param annotation type annotation.
 * @param schema     array schema.
 * @param contains   a type that the array must contain among its items.
 * @param minItems   minimal array length.
 * @param maxItems   maximum array length.
 * @param unique     whether all items must be unique.
 */
final case class JSArray private(annotation: Option[Annotation] = None,
                                 schema: Option[ArraySchema] = None,
                                 contains: Option[JSDataType] = None,
                                 minItems: Option[Int] = None,
                                 maxItems: Option[Int] = None,
                                 unique: Option[Boolean] = None) extends JSDataType {
  assert(minItems.forall(_ >= 0))
  assert(maxItems.forall(_ >= 0))
}

/**
 * Factory for [[JSArray]] instances.
 */
object JSArray extends JsonUtils {

  private def extractSchema(jv: JObject): Option[ArraySchema] = jv \ "items" match {
    case _: JObject => JsonUtils.extractJson[Option[ListType]](jv)
    case _: JArray  => JsonUtils.extractJson[Option[TupleType]](jv)
    case _          => None
  }

  /**
   * JSON serializer for [[JSArray]] instances.
   */
  val serializer = deserializer[JSArray](_ => {
    case jv: JObject =>
      JSArray(
        annotation = Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        schema = extractSchema(jv),
        contains = extractJson[Option[JSDataType]](jv \ "contains"),
        minItems = extractJson[Option[Int]](jv \ "minItems"),
        maxItems = extractJson[Option[Int]](jv \ "maxItems"),
        unique = extractJson[Option[Boolean]](jv \ "unique")
      )
  })
}

/**
 * JSON Schema Object data type.
 *
 * @param annotation           type annotation.
 * @param properties           property types by name.
 * @param patternProperties    property types by name pattern.
 * @param additionalProperties the type of the allowed extra properties. JSON Schema supports
 *                             either a boolean to indicate whether they are allowed or
 *                             a specific type of allowed additional properties.
 *                             We capture this as as generalized [[JSDataType]], where:
 *                             - [[JSAnything]] means any properties are allowed (`true` in JSON schema)
 *                             - [[JSNothing]] means no properties are allowed (`false` in JSON schema)
 *                             - any other type defines which properties are specifically allowed.
 * @param requiredProperties   the required property names.
 * @param propertyNames        enforces constraints on the property names.
 * @param minProperties        the minimum number of properties.
 * @param maxProperties        the maximum number of properties.
 */
final case class JSObject(annotation: Option[Annotation] = None,
                          properties: Option[Map[String, JSDataType]] = None,
                          patternProperties: Option[Map[Pattern, JSDataType]] = None,
                          additionalProperties: Option[JSDataType] = None,
                          requiredProperties: Option[List[String]] = None,
                          propertyNames: Option[JSString] = None,
                          minProperties: Option[Int] = None,
                          maxProperties: Option[Int] = None) extends JSDataType {
  assert(minProperties.forall(_ >= 0))
  assert(maxProperties.forall(_ >= 0))
}

/**
 * Factory for [[JSObject]] instances.
 */
object JSObject extends JsonUtils {

  private def extractAdditionalProps(jv: JValue) = jv match {
    case JBool(true)  => Some(JSAnything)
    case JBool(false) => Some(JSNothing)
    case obj: JObject => Some(extractJson[JSDataType](obj))
    case _            => None
  }

  /**
   * JSON serializer for [[JSObject]] instances.
   */
  val serializer = objectDeserializer[JSObject](jv =>
    JSObject(
      annotation = Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
      properties = extractJson[Option[Map[String, JSDataType]]](jv \ "properties"),
      patternProperties = extractJson[Option[Map[Pattern, JSDataType]]](jv \ "patternProperties"),
      additionalProperties = extractAdditionalProps(jv \ "additionalProperties"),
      requiredProperties = extractJson[Option[List[String]]](jv \ "required"),
      propertyNames = extractJson[Option[JSString]](jv \ "propertyNames"),
      minProperties = extractJson[Option[Int]](jv \ "minProperties"),
      maxProperties = extractJson[Option[Int]](jv \ "maxProperties")
    )
  )
}

/** *
 * JSON Schema Const data type.
 *
 * @param annotation type annotation.
 * @param value      constant value.
 */
final case class JSConst(annotation: Option[Annotation] = None, value: JValue) extends JSScalarType

/**
 * Factory for [[JSConst]] instances.
 */
object JSConst extends JsonUtils {

  /**
   * JSON serializer for [[JSConst]] instances.
   */
  val serializer = deserializer[JSConst](_ => {
    case jv: JObject =>
      JSConst(
        Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        jv \ "const"
      )
  })
}

final case class JSEnum(annotation: Option[Annotation] = None, values: List[JValue]) extends JSScalarType

/**
 * Factory for [[JSEnum]] instances.
 */
object JSEnum extends JsonUtils {

  /**
   * JSON serializer for [[JSEnum]] instances.
   */
  val serializer = deserializer[JSEnum](_ => {
    case jv: JObject =>
      JSEnum(
        Annotation.noneIfEmpty(extractJson[Option[Annotation]](jv)),
        (jv \ "enum").extract[List[JValue]]
      )
  })
}