package com.uralian.jason.util

import com.uralian.jason.AbstractUnitSpec
import enumeratum.EnumEntry.Lowercase
import enumeratum._
import org.json4s.JsonDSL._
import org.json4s.ParserUtil.ParseException
import org.json4s._

import java.util.regex.Pattern

/**
 * JsonUtils test suite.
 */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
class JsonUtilsSpec extends AbstractUnitSpec with JsonUtils {

  import JsonUtilsSpec._

  implicit val jsonFormats: Formats = testFormats

  "parseJson" should {
    "pass valid JSON" in {
      parseJson("""{"a":true, "b":null, "c":[1, 2, null]}""") mustBe
        ("a" -> true) ~ ("b" -> JNull) ~ ("c" -> JArray(List(JInt(1), JInt(2), JNull)))
    }
    "fail for malformed input" in {
      a[ParseException] must be thrownBy parseJson("""{abc}""")
    }
  }

  "renderJson" should {
    "render JSON as a compact string" in {
      val json = ("a" -> true) ~ ("b" -> JNull) ~ ("c" -> JArray(List(JInt(1), JInt(2), JNull)))
      renderJson(json) mustBe """{"a":true,"b":null,"c":[1,2,null]}"""
    }
    "render JSON as a pretty string" in {
      val json = ("a" -> true) ~ ("b" -> JNull) ~ ("c" -> JArray(List(JInt(1), JInt(2), JNull)))
      renderJson(json, false) mustBe
        """{
          |  "a":true,
          |  "b":null,
          |  "c":[1,2,null]
          |}""".stripMargin
    }
  }

  "extractJson" should {
    "convert JSON to class, skipping null fields" in {
      val json = ("name" -> "John") ~ ("married" -> true) ~
        ("employment" -> JNull) ~ ("age" -> JNull) ~ ("scores" -> List(1, 2, 3)) ~ ("tags" -> JNull) ~
        ("children" -> List(
          ("name" -> "Jane") ~ ("married" -> false) ~ ("scores" -> JNull) ~ ("children" -> JNull)
        ))
      extractJson[Person](json) mustBe Person("John", true, None, None, Some(List(1, 2, 3)),
        None, Some(List(Person("Jane", false, None, None, None, None, None))))
    }
    "converting JSON to class, preserving null fields" in {
      val json = ("name" -> JNull) ~ ("married" -> true) ~
        ("employment" -> JNothing) ~ ("age" -> JNull) ~ ("scores" -> List(1, 2, 3)) ~ ("tags" -> JNothing)
      val mf = Manifest.classType[Person](classOf[Person])
      extractJson[Person](json)(jsonFormats.preservingEmptyValues, mf) mustBe Person(null, true, None,
        None, Some(List(1, 2, 3)), None, None)
    }
  }

  "decomposeJson" should {
    "convert class to JSON, skipping null fields" in {
      val person = Person("John", true, Some(Employment.Employed), null, Some(List(1, 2)),
        None, Some(List(Person("Jane", false, None, Some(20), None, null, None)
        )))
      decomposeJson(person) mustBe ("name" -> "John") ~ ("married" -> true) ~
        ("employment" -> "employed") ~ ("scores" -> List(JDecimal(1.0), JDecimal(2.0))) ~ ("children" -> List(
        ("name" -> "Jane") ~ ("married" -> false) ~ ("age" -> 20)
      ))
    }
  }

  "readJson" should {
    "convert string to JSON and then to a class" in {
      val str =
        """{
          |"name": "John",
          |"married": true,
          |"employment": "student",
          |"age": null,
          |"scores": [1, null, 2],
          |"tags": null,
          |"children": [
          |  {"name": "Jane", "gender": "female", "married": false, "scores": null}
          |]
          |}""".stripMargin
      readJson[Person](str) mustBe Person("John", true, Some(Employment.Student), None,
        Some(List(1, 2)), None, Some(List(Person("Jane", false, None, None, None, None, None)
        )))
    }
  }

  "writeJson" should {
    "converts class to JSON and then to a compact string" in {
      val person = Person("John", true, None, null, Some(List(1, 2)), None, Some(List(
        Person("Jane", false, None, Some(20), None, null, None)
      )))
      writeJson(person) mustBe """{"name":"John","married":true,"scores":[1.0,2.0],"children":[{"name":"Jane","married":false,"age":20}]}"""
    }
    "converts class to JSON and then to a pretty string" in {
      val person = Person("John", true, None, null, Some(List(1, 2)), None, Some(List(
        Person("Jane", false, None, Some(20), None, null, None)
      )))
      writeJson(person, false) mustBe
        """{
          |  "name":"John",
          |  "married":true,
          |  "scores":[1.0,2.0],
          |  "children":[{
          |    "name":"Jane",
          |    "married":false,
          |    "age":20
          |  }]
          |}""".stripMargin
    }
  }

  "deserializer" should {
    "create a custom serializer" in {
      val des = deserializer[String](_ => {
        case JInt(n) => n.toString
      })
      implicit val jsonFormats: Formats = testFormats + des
      extractJson[String](JInt(123)) mustBe "123"
    }
    "be ignored when trying to serialize values" in {
      val des = deserializer[String](_ => {
        case JInt(n) => n.toString
      })
      implicit val jsonFormats: Formats = testFormats + des
      decomposeJson("123") mustBe JString("123")
    }
  }

  "PatternSerializer" should {
    val regex = "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
    "deserialize JSON into Pattern" in {
      val json: JValue = regex
      val pattern = extractJson[Pattern](json)
      pattern.pattern() mustBe regex
    }
    "serialize Pattern into JSON string" in {
      val pattern = Pattern.compile(regex)
      val json = decomposeJson(pattern)
      json mustBe JString(regex)
    }
  }

  "PatternKeySerializer" should {
    val regex = "abc.+"
    "deserialize JSON into Pattern key" in {
      val json: JValue = regex -> "hello"
      val data = extractJson[Map[Pattern, String]](json)
      data.map {
        case (k, v) => k.pattern() -> v
      } mustBe Map(regex -> "hello")
    }
    "serialize Pattern into JSON string" in {
      val data = Map(Pattern.compile(regex) -> 123)
      val json = decomposeJson(data)
      json mustBe JObject(regex -> JInt(123))
    }
  }
}

/**
 * Contains test artifacts.
 */
object JsonUtilsSpec {

  sealed trait Employment extends EnumEntry with Lowercase

  object Employment extends Enum[Employment] {
    case object Student extends Employment

    case object Employed extends Employment

    case object Unemployed extends Employment

    case object Retired extends Employment

    val values = findValues
  }

  private final case class Person(name: String,
                                  married: Boolean,
                                  employment: Option[Employment],
                                  age: Option[Int],
                                  scores: Option[List[Double]],
                                  tags: Option[Map[String, Any]],
                                  children: Option[List[Person]])

  private val testFormats = JsonUtils.formats ++
    JsonUtils.commonSerializers + JsonUtils.patternKeySerializer +
    Json4s.serializer(Employment)
}
