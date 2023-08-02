package com.uralian.jason.v7.ast

import com.uralian.jason.util.JsonUtils
import org.json4s.{JValue, JsonInput}

/**
 * JSON Schema root object.
 *
 * @param dataType    target JSON data type defined by this schema.
 * @param draft       schema draft version.
 * @param id          schema identifier.
 * @param definitions auxiliary type definitions.
 */
final case class JsonSchema private(dataType: JSDataType,
                                    draft: Option[String] = None,
                                    id: Option[String] = None,
                                    definitions: Map[String, JSDataType] = Map.empty)

/**
 * Factory for [[JsonSchema]] instances.
 */
object JsonSchema extends JsonUtils {

  /**
   * JSON serializer for [[JsonSchema]] instances.
   */
  val serializer = objectDeserializer[JsonSchema](jv => {

    val defs: Map[String, JSDataType] = extractJson[Option[Map[String, JSDataType]]](jv \ s"$$defs")
      .orElse(extractJson[Option[Map[String, JSDataType]]](jv \ "definitions"))
      .getOrElse(Map.empty)

    JsonSchema(
      dataType = extractJson[JSDataType](jv),
      draft = extractJson[Option[String]](jv \ "$schema"),
      id = extractJson[Option[String]](jv \ "$id"),
      definitions = defs
    )
  })

  /**
   * Creates a JSON Schema instance from a JSON AST.
   *
   * @param json JSON AST.
   * @return a new [[JsonSchema]] instance.
   */
  def apply(json: JValue): JsonSchema = JsonUtils.extractJson[JsonSchema](json)

  /**
   * Creates a JSON Schema instance from a JSON Input.
   *
   * @param json JSON AST.
   * @return a new [[JsonSchema]] instance.
   */
  def apply(json: JsonInput): JsonSchema = JsonUtils.readJson[JsonSchema](json)

  /**
   * Creates a JSON Schema instance from a JSON string.
   *
   * @param json JSON string.
   * @return a new [[JsonSchema]] instance.
   */
  def apply(json: String): JsonSchema = JsonUtils.readJson[JsonSchema](json)
}