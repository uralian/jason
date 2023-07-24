package com.uralian.jason.v7.ast

import enumeratum._

/**
 * JSON Schema type name.
 *
 * @param entryName JSON Schema type literal.
 */
sealed abstract class JSTypeName(override val entryName: String) extends EnumEntry

/**
 * Supported JSON Schema type names.
 */
object JSTypeName extends Enum[JSTypeName] {

  case object JSString extends JSTypeName("string")

  case object JSNumber extends JSTypeName("number")

  case object JSInteger extends JSTypeName("integer")

  case object JSObject extends JSTypeName("object")

  case object JSArray extends JSTypeName("array")

  case object JSBoolean extends JSTypeName("boolean")

  case object JSNull extends JSTypeName("null")

  val values = findValues
}