package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.MappingException

/**
 * JSObject test suite.
 */
class JSObjectSpec extends AbstractUnitSpec {

  "JSObject.serializer" should {
    "deserialize from valid JSON with typed additional properties" in {
      val json =
        """
          |{
          |  "type": "object",
          |  "title": "person",
          |  "properties": {
          |    "name": {"type": "string", "maxLength": 100},
          |    "age": {"type": "integer", "minimum": 0}
          |  },
          |  "patternProperties": {
          |    "^S_": {"type": "string"},
          |    "^B_": {"type": "boolean"}
          |  },
          |  "additionalProperties": {"type": "array"},
          |  "required": ["name", "age"],
          |  "propertyNames": {
          |    "minLength": 10,
          |    "maxLength": 50
          |  },
          |  "minProperties": 2,
          |  "maxProperties": 5
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSObject](json)
      data.annotation.value mustBe Annotation(title = Some("person"))
      data.properties mustBe Map[String, JSDataType](
        "name" -> JSString(maxLength = Some(100)),
        "age" -> JSInteger(minimum = Some(0))
      )
      data.patternProperties.map {
        case (k, v) => k.pattern() -> v
      } mustBe Map[String, JSDataType]("^S_" -> JSString(), "^B_" -> JSBoolean())
      data.additionalProperties.value mustBe JSArray()
      data.requiredProperties mustBe List("name", "age")
      data.propertyNames.value mustBe JSString(minLength = Some(10), maxLength = Some(50))
      data.minProperties.value mustBe 2
      data.maxProperties.value mustBe 5
    }
    "deserialize from valid JSON with allowed additional properties" in {
      val json =
        """
          |{
          |  "type": "object",
          |  "additionalProperties": true
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSObject](json)
      data.additionalProperties.value mustBe JSAnything
    }
    "deserialize from valid JSON with disallowed additional properties" in {
      val json =
        """
          |{
          |  "type": "object",
          |  "additionalProperties": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSObject](json)
      data.additionalProperties.value mustBe JSNothing
    }
    "deserialize from valid JSON with unspecified additional properties" in {
      val json =
        """
          |{
          |  "type": "object"
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSObject](json)
      data.additionalProperties mustBe empty
    }
    "fail for invalid JSON" in {
      val json = """{"minProperties": true}"""
      a[MappingException] mustBe thrownBy(JsonUtils.readJson[JSObject](json))
    }
    "fail for bad parameters" in {
      val json = """{"minProperties": -1}"""
      an[AssertionError] mustBe thrownBy(JsonUtils.readJson[JSObject](json))
    }
  }
}
