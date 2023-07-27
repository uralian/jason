package com.uralian.jason.v7

import com.uralian.jason.util.JsonUtils
import enumeratum.Json4s
import org.json4s.{Formats, Serializer}

/**
 * Helper methods and types for V7 AST.
 */
package object ast {

  private val primitiveTypeSerializers = List[Serializer[_]](
    JSString.serializer,
    JSInteger.serializer,
    JSNumber.serializer,
    JSBoolean.serializer,
    JSNull.serializer
  )

  /**
   * JSON formats.
   */
  implicit val jsonFormats: Formats = JsonUtils.formats ++
    JsonUtils.commonSerializers +
    JsonUtils.patternKeySerializer ++
    primitiveTypeSerializers +
    ListType.serializer +
    TupleType.serializer +
    JSArray.serializer +
    JSObject.serializer +
    Annotation.serializer +
    JSDataType.serializer +
    Json4s.serializer(JSStringFormat)
}
