package com.uralian.jason.v7.ast

import enumeratum._
import org.json4s.FieldSerializer
import org.json4s.FieldSerializer.renameFrom

/**
 * Content encoding for non-string data.
 *
 * @param entryName encoding name as per RFC 2054, part 6.1 and RFC 4648.
 */
sealed abstract class ContentEncoding(override val entryName: String) extends EnumEntry

/**
 * Available content encodings.
 */
object ContentEncoding extends Enum[ContentEncoding] {

  case object Bit7 extends ContentEncoding("7bit")

  case object Bit8 extends ContentEncoding("8bit")

  case object Binary extends ContentEncoding("binary")

  case object QuotedPrintable extends ContentEncoding("quoted-printable")

  case object Base16 extends ContentEncoding("base16")

  case object Base32 extends ContentEncoding("base32")

  case object Base64 extends ContentEncoding("base64")

  val values = findValues
}

/**
 * Media content information.
 *
 * @param mediaType media type, as per RFC 2046.
 * @param encoding  media encoding method.
 */
final case class MediaContent(mediaType: Option[String], encoding: Option[ContentEncoding])

/**
 * Factory for [[MediaContent]] instances.
 */
object MediaContent {
  /**
   * JSON serializer for [[MediaContent]] instances.
   */
  val serializer = FieldSerializer[MediaContent](
    deserializer = renameFrom("contentMediaType", "mediaType") orElse
      renameFrom("contentEncoding", "encoding")
  )

  /**
   * Checks if the [[MediaContent]] instance contains any information.
   *
   * @param content media content to check.
   * @return `Some(MediaContent)` if the content is not empty, `None` otherwise.
   */
  def noneIfEmpty(content: Option[MediaContent]): Option[MediaContent] = {
    content.filter(mc => mc.mediaType.isDefined || mc.encoding.isDefined)
  }
}
