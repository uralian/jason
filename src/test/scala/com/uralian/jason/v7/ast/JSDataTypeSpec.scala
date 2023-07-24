package com.uralian.jason.v7.ast

import com.uralian.jason.AbstractUnitSpec
import com.uralian.jason.util.JsonUtils
import org.json4s.JsonDSL._
import org.json4s.{JArray, JInt}

/**
 * JSDataType test suite.
 */
class JSDataTypeSpec extends AbstractUnitSpec {

  "Annotation" should {
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
      data.title mustBe None
      data.description mustBe None
      data.default mustBe None
      data.examples mustBe None
      data.comment mustBe None
      data.readOnly mustBe None
      data.writeOnly mustBe None
    }
  }
}
