package com.uralian.jason.util

import com.google.common.base.CaseFormat

import scala.util.Random

/**
 * Various routines for String manipulation.
 */
trait StringUtils {

  /**
   * Converts a string to a valid entry name for enum constant.
   *
   * @param str string to convert.
   * @return the resulting enum constant.
   */
  def toEnumEntryName(str: String) = {
    val id = toValidId(str)
    CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, id.toUpperCase)
  }

  /**
   * Converts a string into a valid identifier. If the argument is empty after removing
   * invalid characters, it generates a new random identifier.
   *
   * @param str  string to convert.
   * @param size if the argument cannot be converted, defines the size of the identifier to generate.
   * @return a valid identifier.
   */
  def toValidId(str: String, size: Int = 20) = {
    val base = str
      .replaceAll("\\W+", "_")
      .replaceAll("_+", "_")
    base.headOption match {
      case Some('_')              => "ID" + base
      case Some(ch) if ch.isDigit => "ID_" + base
      case Some(_)                => base
      case None                   => Random.alphanumeric.take(size).mkString
    }
  }
}

/**
 * Singleton object for String utilities.
 */
object StringUtils extends StringUtils