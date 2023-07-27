package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.JsonDSL._
import org.json4s.{JArray, JInt, JValue}

/**
 * JSDataType test suite.
 */
class JSDataTypeSpec extends AbstractUnitSpec {

  "Annotation.serializer" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "title": "sample",
          |  "description": "sample type",
          |  "default": 123,
          |  "examples": [
          |     456,
          |     "blah",
          |     false,
          |     {"a": "b"}
          |  ]
          |  "$comment": "no comment",
          |  "readOnly": true,
          |  "writeOnly": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[Annotation](json)
      data.title.value mustBe "sample"
      data.description.value mustBe "sample type"
      data.default.value mustBe JInt(123)
      data.examples.value mustBe JArray(List(456, "blah", false, "a" -> "b"))
      data.comment.value mustBe "no comment"
      data.readOnly.value mustBe true
      data.writeOnly.value mustBe false
    }
    "deserialize from valid JSON without optional elements" in {
      val json = "{}"
      val data = JsonUtils.readJson[Annotation](json)
      data.title mustBe empty
      data.description mustBe empty
      data.default mustBe empty
      data.examples mustBe empty
      data.comment mustBe empty
      data.readOnly mustBe empty
      data.writeOnly mustBe empty
    }
    "Annotation.noneIfEmpty() empty Annotations" in {
      Annotation.noneIfEmpty(None) mustBe empty
      Annotation.noneIfEmpty(Some(Annotation())) mustBe empty
      Annotation.noneIfEmpty(Some(Annotation(title = Some("sample")))) mustBe defined
    }
  }

  "JSDataType.serializer" should {
    "handle 'true' schema" in {
      val jv: JValue = true
      JsonUtils.extractJson[JSDataType](jv) mustBe JSAnything
    }
    "handle 'false' schema" in {
      val jv: JValue = false
      JsonUtils.extractJson[JSDataType](jv) mustBe JSNothing
    }
    "handle explicit 'string' schema" in {
      val json =
        """
          |{
          |  "type": "string",
          |  "title": "sample",
          |  "format": "date"
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSString(Some(_), _, _, _, Some(fmt)) => fmt mustBe JSStringFormat.Date
      }
    }
    "handle explicit 'integer' schema" in {
      val json =
        """
          |{
          |  "type": "integer",
          |  "multipleOf": 2
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSInteger(_, m, _, _, _, _) => m.value mustBe 2
      }
    }
    "handle explicit 'number' schema" in {
      val json =
        """
          |{
          |  "type": "number",
          |  "minimum": 5
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSNumber(_, _, m, _, _, _) => m.value mustBe 5
      }
    }
    "handle explicit 'boolean' schema" in {
      val json =
        """
          |{
          |  "type": "boolean",
          |  "description": "sample type"
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSBoolean(Some(ann)) => ann.description.value mustBe "sample type"
      }
    }
    "handle explicit 'null' schema" in {
      val json =
        """
          |{
          |  "type": "null",
          |  "description": "sample type"
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSNull(Some(ann)) => ann.description.value mustBe "sample type"
      }
    }
    "handle explicit 'array' schema" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {"type": "integer"},
          |  "unique": true
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSArray(_, schema, _, _, _, unique) =>
          schema.value mustBe ListType(JSInteger())
          unique.value mustBe true
      }
    }
    "handle explicit 'object' schema" in {
      val json =
        """
          |{
          |  "type": "object",
          |  "properties": {
          |    "name": {"type": "string", "maxLength": 100},
          |    "age": {"type": "integer", "minimum": 0}
          |  },
          |  "minProperties": 2
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSObject(_, props, _, _, _, _, minProps, _) =>
          props.value mustBe Map[String, JSDataType](
            "name" -> JSString(maxLength = Some(100)),
            "age" -> JSInteger(minimum = Some(0))
          )
          minProps.value mustBe 2
      }
    }
    "handle implicit 'string' schema" in {
      val json =
        """
          |{
          |  "minLength": 7
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSString(_, ml, _, _, _) => ml.value mustBe 7
      }
    }
    "handle implicit 'number' schema" in {
      val json =
        """
          |{
          |  "maximum": 100
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSNumber(_, _, _, _, m, _) => m.value mustBe 100
      }
    }
    "handle implicit 'array' schema" in {
      val json =
        """
          |{
          |  "items": {"type": "integer"},
          |  "contains": {"type": "integer", "minimum": 0}
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSArray(_, schema, contains, _, _, _) =>
          schema.value mustBe ListType(JSInteger())
          contains.value mustBe JSInteger(minimum = Some(0))
      }
    }
    "handle implicit 'object' schema" in {
      val json =
        """
          |{
          |  "properties": {
          |    "name": {"type": "string", "maxLength": 100},
          |    "age": {"type": "integer", "minimum": 0}
          |  },
          |  "maxProperties": 5
          |}
          |""".stripMargin
      inside(JsonUtils.readJson[JSDataType](json)) {
        case JSObject(_, props, _, _, _, _, _, maxProps) =>
          props.value mustBe Map[String, JSDataType](
            "name" -> JSString(maxLength = Some(100)),
            "age" -> JSInteger(minimum = Some(0))
          )
          maxProps.value mustBe 5
      }
    }
    "handle default 'accept all' schema" in {
      val json = """{}"""
      JsonUtils.readJson[JSDataType](json) mustBe JSAnything
    }
  }
}
