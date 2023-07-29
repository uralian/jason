package com.uralian.jason.v7

import com.uralian.jason.util.JsonUtils
import enumeratum.Json4s
import org.json4s.{Formats, Serializer}

/**
 * Helper methods and types for V7 AST.
 */
package object ast {

  private val scalarTypeSerializers = List[Serializer[_]](
    JSString.serializer,
    JSInteger.serializer,
    JSNumber.serializer,
    JSBoolean.serializer,
    JSNull.serializer,
    JSConst.serializer,
    JSEnum.serializer
  )

  private val compositeTypeSerializers = List[Serializer[_]](
    JSArray.serializer,
    JSObject.serializer,
    JSAllOf.serializer,
    JSAnyOf.serializer,
    JSOneOf.serializer,
    JSNot.serializer
  )

  /**
   * JSON formats.
   */
  implicit val jsonFormats: Formats = JsonUtils.formats ++
    JsonUtils.commonSerializers +
    JsonUtils.patternKeySerializer +
    MediaContent.serializer ++
    scalarTypeSerializers +
    ListType.serializer +
    TupleType.serializer ++
    compositeTypeSerializers +
    Annotation.serializer +
    JSDataType.serializer +
    Json4s.serializer(JSStringFormat) +
    Json4s.serializer(ContentEncoding)
}
