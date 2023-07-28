package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.MappingException

/**
 * JSArray test suite.
 */
class JSArraySpec extends AbstractUnitSpec {

  "ListType.serializer" should {
    "deserialize from valid JSON" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {"type": "number", "minimum": 5}
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[ListType](json)
      data.itemType mustBe JSNumber(minimum = Some(5))
    }
  }

  "TupleType.serializer" should {
    "deserialize from valid JSON with additional items" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": [
          |    {"type": "number"},
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ],
          |  "additionalItems": true
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[TupleType](json)
      data.itemTypes mustBe List[JSDataType](JSNumber(), JSString(), JSBoolean())
      data.allowExtra.value mustBe JSAnything
    }
    "deserialize from valid JSON with no additional items" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": [
          |    {"type": "number"},
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ],
          |  "additionalItems": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[TupleType](json)
      data.itemTypes mustBe List[JSDataType](JSNumber(), JSString(), JSBoolean())
      data.allowExtra.value mustBe JSNothing
    }
    "deserialize from valid JSON with typed additional items" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": [
          |    {"type": "number"},
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ],
          |  "additionalItems": {"type": "number"}
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[TupleType](json)
      data.itemTypes mustBe List[JSDataType](JSNumber(), JSString(), JSBoolean())
      data.allowExtra.value mustBe JSNumber()
    }
    "deserialize from valid JSON with additional items not specified" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": [
          |    {"type": "number"},
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ]
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[TupleType](json)
      data.itemTypes mustBe List[JSDataType](JSNumber(), JSString(), JSBoolean())
      data.allowExtra mustBe empty
    }
  }

  "JSArray.serializer" should {
    "deserialize list-array from valid JSON" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "title": "sample",
          |  "items": {"type": "integer", "$comment": "something", "multipleOf": 10},
          |  "contains": {"type": "integer"},
          |  "minItems": 2,
          |  "maxItems": 8,
          |  "unique": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSArray](json)
      inside(data.schema.value) {
        case ListType(JSInteger(ann, mo, _, _, _, _)) =>
          ann.value mustBe Annotation(comment = Some("something"))
          mo.value mustBe 10
      }
      data.annotation.value mustBe Annotation(title = Some("sample"))
      data.contains.value mustBe JSInteger()
      data.minItems.value mustBe 2
      data.maxItems.value mustBe 8
      data.unique.value mustBe false
    }
    "deserialize tuple-array from valid JSON" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "description": "sample array",
          |  "items": [
          |    {"type": "number"},
          |    {"type": "string"},
          |    {"type": "boolean"}
          |  ],
          |  "minItems": 1,
          |  "maxItems": 5,
          |  "additionalItems": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSArray](json)
      inside(data.schema.value) {
        case TupleType(itemTypes, allowExtra) =>
          itemTypes mustBe List[JSDataType](JSNumber(), JSString(), JSBoolean())
          allowExtra.value mustBe JSNothing
      }
      data.annotation.value mustBe Annotation(description = Some("sample array"))
      data.minItems.value mustBe 1
      data.maxItems.value mustBe 5
    }
    "deserialize schemaless array from valid JSON" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "title": "sample",
          |  "contains": {"type": "integer"},
          |  "minItems": 2,
          |  "maxItems": 8,
          |  "unique": false
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSArray](json)
      data.schema mustBe empty
      data.annotation.value mustBe Annotation(title = Some("sample"))
      data.contains.value mustBe JSInteger()
      data.minItems.value mustBe 2
      data.maxItems.value mustBe 8
      data.unique.value mustBe false
    }
    "deserialize array with complex type items" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {"type": "array", "unique": true, "items": {"type": "string"}},
          |}
          |""".stripMargin
      val data = JsonUtils.readJson[JSArray](json)
      inside(data.schema.value) {
        case ListType(JSArray(_, schema, _, _, _, unique)) =>
          schema.value mustBe ListType(JSString())
          unique.value mustBe true
      }
    }
    "fail for invalid JSON" in {
      val json = """{"minItems": true}"""
      a[MappingException] mustBe thrownBy(JsonUtils.readJson[JSArray](json))
    }
    "fail for bad parameters" in {
      val json = """{"minItems": -1}"""
      an[AssertionError] mustBe thrownBy(JsonUtils.readJson[JSArray](json))
    }
  }
}
