package com.uralian.jason.util

import com.uralian.jason.AbstractUnitSpec

/**
 * StringUtils test suite.
 */
class StringUtilsSpec extends AbstractUnitSpec with StringUtils {

  "toValidId" should {
    "convert string to valid ID" in {
      toValidId("abc123") mustBe "abc123"
      toValidId("123abc") mustBe "ID_123abc"
      toValidId("a ;b _1++2-3") mustBe "a_b_1_2_3"
    }
    "produce valid ID for any string" in
      forAll() { (str: String) =>
        val id = toValidId(str)
        id must not be empty
        id.forall(c => c.isLetterOrDigit || c == '_')
        id.headOption foreach (_.isLetter)
      }
  }

  "toEnumEntryName" should {
    "convert string to valid enum entry name" in {
      toEnumEntryName("abc123") mustBe "Abc123"
      toEnumEntryName("123abc") mustBe "Id123abc"
      toEnumEntryName("ab ;c _1++2-3") mustBe "AbC123"
    }
    "produce valid enum entry name for any string" in
      forAll("s") { (str: String) =>
        val name = toEnumEntryName(str)
        name must not be empty
        name.forall(c => c.isLetterOrDigit || c == '_')
        name.headOption foreach (c => c.isLetter && c.isUpper)
      }
  }
}
