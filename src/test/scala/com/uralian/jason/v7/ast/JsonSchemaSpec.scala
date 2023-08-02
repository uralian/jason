package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._

/**
 * JSON Schema test suite.
 */
class JsonSchemaSpec extends AbstractUnitSpec {

  "JsonSchema.serializer" should {
    "deserialize from valid JSON" in {
      val json =
        s"""
           |{
           |  "$$schema": "http://json-schema.org/draft-07/schema#",
           |  "$$id": "/schemas/pipeline",
           |  "description": "Data Pipeline",
           |
           |  "definitions": {
           |    "UserType": {
           |      "type": "string",
           |      "oneOf": [
           |        {"const": "USER"},
           |        {"const": "GROUP"}
           |      ]
           |    },
           |    "MergeOptions": {
           |      "type": "object",
           |      "properties": {
           |        "key": {
           |          "type": "array",
           |          "items": {"type": "string"}
           |        },
           |        "timestamp": {
           |          "type": "string",
           |          "description": "Identifies last version"
           |        }
           |      },
           |      "required": ["key"]
           |    }
           |  },
           |
           |  "type": "object",
           |
           |  "properties": {
           |    "merge": {"$$ref": "#/definitions/MergeOptions"},
           |    "userType": {"$$ref": "#/definitions/UserType"}
           |  },
           |
           |  "required": ["merge"],
           |}
           |""".stripMargin

      val schema = JsonUtils.readJson[JsonSchema](json)
      schema.draft.value mustBe "http://json-schema.org/draft-07/schema#"
      schema.id.value mustBe "/schemas/pipeline"

      val userType = JSOneOf(None, List(JSConst(value = "USER"), JSConst(value = "GROUP")))
      val mergeOptions = JSObject(
        properties = Map[String, JSDataType](
          "key" -> JSArray(schema = Some(ListType(JSString()))),
          "timestamp" -> JSString(annotation = Some(Annotation(description = Some("Identifies last version"))))
        ),
        requiredProperties = List("key")
      )
      schema.definitions mustBe Map[String, JSDataType]("UserType" -> userType, "MergeOptions" -> mergeOptions)

      schema.dataType mustBe JSObject(
        annotation = Some(Annotation(description = Some("Data Pipeline"))),
        properties = Map[String, JSDataType](
          "merge" -> JSRef("#/definitions/MergeOptions"),
          "userType" -> JSRef("#/definitions/UserType")
        ),
        requiredProperties = List("merge")
      )
    }
    "deserialize schema without definitions from valid JSON" in {
      val json =
        s"""
           |{
           |  "$$id": "/schemas/number",
           |  "type": "number"
           |}
           |""".stripMargin
      val schema = JsonUtils.readJson[JsonSchema](json)
      schema.id.value mustBe "/schemas/number"
      schema.definitions mustBe empty
      schema.dataType mustBe JSNumber()
    }
  }

  "JsonSchema.apply" should {
    "create schema from JValue" in {
      val json: JValue = ("$id" -> "/schemas/pipeline") ~ ("type" -> "object") ~
        ("properties" -> ("name" -> ("type" -> "string")))
      val schema = JsonSchema(json)
      schema.id.value mustBe "/schemas/pipeline"
      schema.dataType mustBe JSObject(properties = Map("name" -> JSString()))
    }
    "create schema from String" in {
      val json =
        s"""
           |{
           |  "title": "sample",
           |  "properties": {
           |    "name": {
           |      "type": "string",
           |      "minLength": 1
           |    },
           |    "age": {
           |      "type": "integer",
           |      "minimum": 18
           |    }
           |  }
           |}
           |""".stripMargin
      val schema = JsonSchema(json)
      schema.dataType mustBe JSObject(
        annotation = Some(Annotation(title = Some("sample"))),
        properties = Map[String, JSDataType](
          "name" -> JSString(minLength = Some(1)),
          "age" -> JSInteger(minimum = Some(18))
        )
      )
    }
    "create schema from JsonInput" in {
      val input = getClass.getResourceAsStream("/lego.json")
      val schema = JsonSchema(input)
      schema.draft.value mustBe "http://json-schema.org/draft-04/schema#"

      def description(str: String) = Some(Annotation(description = Some(str)))

      val blocks = JSArray(
        annotation = Some(Annotation(description = Some("An array of your app's blocks."))),
        schema = Some(ListType(JSObject(
          properties = Map[String, JSDataType](
            "type" -> JSString(annotation = description("The name of the block type")),
            "path" -> JSString(annotation = description("Path relative to root where the block will be stored")),
            "isFile" -> JSBoolean(annotation = description("Is this a file? or a folder? False by default.")),
            "files" -> JSArray(
              annotation = description("Files making up the block"),
              schema = Some(ListType(JSRef("#/$defs/file")))
            ),
            "file" -> JSRef("#/$defs/file")
          ),
          requiredProperties = List("type", "path")
        )))
      )

      inside(schema.dataType) {
        case JSObject(_, props, _, _, required, _, _, _) =>
          props.size mustBe 2
          props.get("blocks").value mustBe blocks
          inside(props.get("fileFormats").value) {
            case obj: JSObject =>
              obj.annotation mustBe description("An object mapping file objects to strings")
              obj.patternProperties.map {
                case (k, v) => k.pattern() -> v
              } mustBe Map(".*" -> JSRef("#/$defs/fileFormat"))
          }
          required mustBe List("blocks")
      }

      schema.definitions mustBe Map[String, JSDataType](
        "file" -> JSOneOf(dataTypes = List[JSDataType](
          JSString(annotation = description("This object or string represents the file format")),
          JSNull(annotation = description("This object or string represents the file format")),
          JSRef("#/$defs/fileFormat")
        )),
        "fileFormat" -> JSObject(
          annotation = description("This object represents a file format"),
          properties = Map(
            "name" -> JSString(annotation = description("Name of block file")),
            "template" -> JSString(annotation = description("Path to file template"))
          ),
          requiredProperties = List("name")
        )
      )
    }
  }
}
