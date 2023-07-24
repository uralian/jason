package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.{JArray, JDecimal, JInt, MappingException}

/**
 * JSDataType test suite.
 */
class JSPrimitiveTypeSpec extends AbstractUnitSpec {

  "JSString" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "type": "string",
          |  "title": "sample",
          |  "description": "sample type",
          |  "$comment": "sample comment",
          |  "minLength": 2,
          |  "maxLength": 5,
          |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$",
          |  "format": "email"
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSString](json)
      val annotation = Annotation(title = Some("sample"),
        description = Some("sample type"),
        comment = Some("sample comment")
      )
      data.annotation.value mustBe annotation
      data.minLength.value mustBe 2
      data.maxLength.value mustBe 5
      data.pattern.value.pattern() mustBe "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
      data.format.value mustBe JSStringFormat.Email
    }
    "deserialize from valid JSON without optional elements" in {
      val json = """{}"""
      val data = JsonUtils.readJson[JSString](json)
      data.typeName mustBe JSTypeName.JSString
      data.annotation mustBe None
      data.minLength mustBe None
      data.maxLength mustBe None
      data.pattern mustBe None
      data.format mustBe None
    }
    "fail for invalid JSON" in {
      val json = """{"minLength": true}"""
      a[MappingException] mustBe thrownBy(JsonUtils.readJson[JSString](json))
    }
    "fail for bad parameters" in {
      val json = """{"minLength": -1}"""
      an[AssertionError] mustBe thrownBy(JsonUtils.readJson[JSString](json))
    }
  }

  "JSInteger" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "type": "integer",
          |  "title": "sample",
          |  "description": "sample type",
          |  "$comment": "sample comment",
          |  "minimum": 3,
          |  "maximum": 8,
          |  "exclusiveMinimum": 4,
          |  "exclusiveMaximum": 7,
          |  "multipleOf": 5.5
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSInteger](json)
      val annotation = Annotation(title = Some("sample"),
        description = Some("sample type"),
        comment = Some("sample comment")
      )
      data.annotation.value mustBe annotation
      data.minimum.value mustBe 3
      data.maximum.value mustBe 8
      data.exclusiveMinimum.value mustBe 4
      data.exclusiveMaximum.value mustBe 7
      data.multipleOf.value mustBe 5.5
    }
    "deserialize from valid JSON without optional elements" in {
      val json = """{}"""
      val data = JsonUtils.readJson[JSInteger](json)
      data.typeName mustBe JSTypeName.JSInteger
      data.annotation mustBe None
      data.minimum mustBe None
      data.maximum mustBe None
      data.exclusiveMinimum mustBe None
      data.exclusiveMaximum mustBe None
      data.multipleOf mustBe None
    }
    "fail for invalid JSON" in {
      val json = """{"minimum": "11"}"""
      a[MappingException] mustBe thrownBy(JsonUtils.readJson[JSInteger](json))
    }
    "fail for bad parameters" in {
      val json = """{"multipleOf": -2}"""
      an[AssertionError] mustBe thrownBy(JsonUtils.readJson[JSInteger](json))
    }
  }

  "JSNumber" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "type": "number",
          |  "title": "sample",
          |  "default": 12.34,
          |  "examples": [1, 2, 3.4],
          |  "minimum": 3,
          |  "maximum": 8,
          |  "exclusiveMinimum": 4,
          |  "exclusiveMaximum": 7,
          |  "multipleOf": 5.5
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSNumber](json)
      val annotation = Annotation(title = Some("sample"),
        default = Some(JDecimal(12.34)),
        examples = Some(JArray(List(JInt(1), JInt(2), JDecimal(3.4))))
      )
      data.annotation.value mustBe annotation
      data.minimum.value mustBe 3
      data.maximum.value mustBe 8
      data.exclusiveMinimum.value mustBe 4
      data.exclusiveMaximum.value mustBe 7
      data.multipleOf.value mustBe 5.5
    }
    "deserialize from valid JSON without optional elements" in {
      val json = """{}"""
      val data = JsonUtils.readJson[JSNumber](json)
      data.typeName mustBe JSTypeName.JSNumber
      data.minimum mustBe None
      data.maximum mustBe None
      data.exclusiveMinimum mustBe None
      data.exclusiveMaximum mustBe None
      data.multipleOf mustBe None
    }
    "fail for invalid JSON" in {
      val json = """{"maximum": "11"}"""
      a[MappingException] mustBe thrownBy(JsonUtils.readJson[JSNumber](json))
    }
    "fail for bad parameters" in {
      val json = """{"multipleOf": -2}"""
      an[AssertionError] mustBe thrownBy(JsonUtils.readJson[JSNumber](json))
    }
  }

  "JSBoolean" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "title": "sample",
          |  "description": "sample type"
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSBoolean](json)
      data.annotation.value mustBe Annotation(title = Some("sample"), description = Some("sample type"))
    }
    "deserialize from valid JSON without optional elements" in {
      val json = """{}"""
      val data = JsonUtils.readJson[JSBoolean](json)
      data.typeName mustBe JSTypeName.JSBoolean
      data.annotation mustBe None
    }
  }

  "JSNull" should {
    "deserialize from valid JSON with optional elements" in {
      val json =
        """
          |{
          |  "title": "sample",
          |  "description": "sample type"
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSNull](json)
      data.annotation.value mustBe Annotation(title = Some("sample"), description = Some("sample type"))
    }
    "deserialize from valid JSON without optional elements" in {
      val json = """{}"""
      val data = JsonUtils.readJson[JSNull](json)
      data.typeName mustBe JSTypeName.JSNull
      data.annotation mustBe None
    }
  }
}