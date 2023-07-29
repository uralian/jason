package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils

/**
 * JSCompositeType test suite.
 */
class JSCompositeTypeSpec extends AbstractUnitSpec {

  "JSAllOf.serializer" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "$comment": "sample comment",
          |  "allOf": [
          |    {"type": "string"},
          |    {"minLength": 1},
          |    {"maxLength": 5}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAllOf](json)
      data.annotation.value mustBe Annotation(comment = Some("sample comment"))
      data.dataTypes mustBe List[JSDataType](
        JSString(), JSString(minLength = Some(1)), JSString(maxLength = Some(5)))
    }
    "deserialize from valid JSON without optional elements" in {
      val json =
        """
          |{
          |  "allOf": []
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAllOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe empty
    }
  }

  "JSAnyOf.serializer" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "$comment": "sample comment",
          |  "anyOf": [
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAnyOf](json)
      data.annotation.value mustBe Annotation(comment = Some("sample comment"))
      data.dataTypes mustBe List[JSDataType](JSString(), JSBoolean())
    }
    "deserialize from valid JSON without optional elements" in {
      val json =
        """
          |{
          |  "anyOf": []
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSAnyOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe empty
    }
  }

  "JSOneOf.serializer" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "$comment": "sample comment",
          |  "oneOf": [
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSOneOf](json)
      data.annotation.value mustBe Annotation(comment = Some("sample comment"))
      data.dataTypes mustBe List[JSDataType](JSString(), JSBoolean())
    }
    "deserialize from valid JSON without optional elements" in {
      val json =
        """
          |{
          |  "oneOf": []
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSOneOf](json)
      data.annotation mustBe empty
      data.dataTypes mustBe empty
    }
  }

  "JSNot.serializer" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "$comment": "sample comment",
          |  "not": {"type": "number"}
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSNot](json)
      data.annotation.value mustBe Annotation(comment = Some("sample comment"))
      data.dataType mustBe JSNumber()
    }
    "deserialize from valid JSON without optional elements" in {
      val json =
        """
          |{
          |  "not": {"type": "array"}
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSNot](json)
      data.annotation mustBe empty
      data.dataType mustBe JSArray()
    }
  }
}
