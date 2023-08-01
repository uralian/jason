package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils

/**
 * JSCompositeType test suite.
 */
class JSCompositeTypeSpec extends AbstractUnitSpec {

  "JSAllOf.serializer" should {
    "deserialize from valid JSON without common elements" in {
      val json =
        """
          |{
          |  "title": "all",
          |  "description": "all of them",
          |  "allOf": [
          |    {"type": "string"},
          |    {"minLength": 1},
          |    {"maxLength": 5}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAllOf](json)
      data.annotation.value mustBe Annotation(title = Some("all"), description = Some("all of them"))
      data.dataTypes mustBe List[JSDataType](
        JSString(), JSString(minLength = Some(1)), JSString(maxLength = Some(5))
      )
    }
    "deserialize from valid JSON with common elements" in {
      val json =
        """
          |{
          |  "type": "string",
          |  "maxLength": 100,
          |  "allOf": [
          |    {"minLength": 5},
          |    {"format": "email", "maxLength": 10},
          |    {"maxLength": 20}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAllOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe List[JSDataType](
        JSString(maxLength = Some(100), minLength = Some(5)),
        JSString(maxLength = Some(10), format = Some(JSStringFormat.Email)),
        JSString(maxLength = Some(20))
      )
    }
  }

  "JSAnyOf.serializer" should {
    "deserialize from valid JSON without common elements" in {
      val json =
        """
          |{
          |  "title": "any",
          |  "description": "any of them",
          |  "anyOf": [
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAnyOf](json)
      data.annotation.value mustBe Annotation(title = Some("any"), description = Some("any of them"))
      data.dataTypes mustBe List[JSDataType](JSString(), JSBoolean())
    }
    "deserialize from valid JSON with common elements" in {
      val json =
        """
          |{
          |  "type": "integer",
          |  "minimum": 0,
          |  "anyOf": [
          |    {"maximum": 100, "minimum": 10},
          |    {"multipleOf": 10}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAnyOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe List[JSDataType](
        JSInteger(minimum = Some(10), maximum = Some(100)),
        JSInteger(multipleOf = Some(10), minimum = Some(0))
      )
    }
  }

  "JSOneOf.serializer" should {
    "deserialize from valid JSON without common elements" in {
      val json =
        """
          |{
          |  "title": "one",
          |  "description": "one of them",
          |  "oneOf": [
          |    {"type": "string"},
          |    {"type": "number"}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSOneOf](json)
      data.annotation.value mustBe Annotation(title = Some("one"), description = Some("one of them"))
      data.dataTypes mustBe List[JSDataType](JSString(), JSNumber())
    }
    "deserialize from valid JSON with common elements" in {
      val json =
        """
          |{
          |  "type": "integer",
          |  "minimum": 0,
          |  "oneOf": [
          |    {"multipleOf": 2, "minimum": 10},
          |    {"multipleOf": 3},
          |    {"multipleOf": 5},
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSOneOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe List[JSDataType](
        JSInteger(minimum = Some(10), multipleOf = Some(2)),
        JSInteger(minimum = Some(0), multipleOf = Some(3)),
        JSInteger(minimum = Some(0), multipleOf = Some(5))
      )
    }
  }
}
