package com.uralian.jason.v7.ast

import enumeratum._

/**
 * JSON String format.
 *
 * @param entryName
 */
sealed abstract class JSStringFormat(override val entryName: String) extends EnumEntry

/**
 * Available string formats.
 */
object JSStringFormat extends Enum[JSStringFormat] {

  case object DateTime extends JSStringFormat("date-time")

  case object Date extends JSStringFormat("date")

  case object Time extends JSStringFormat("time")

  case object Email extends JSStringFormat("email")

  case object IDNEmail extends JSStringFormat("idn-email")

  case object Hostname extends JSStringFormat("hostname")

  case object IDNHostname extends JSStringFormat("idn-hostname")

  case object IPv4 extends JSStringFormat("ipv4")

  case object IPv6 extends JSStringFormat("ipv6")

  case object UUID extends JSStringFormat("uuid")

  case object URI extends JSStringFormat("uri")

  case object URIReference extends JSStringFormat("uri-reference")

  case object IRI extends JSStringFormat("iri")

  case object IRIReference extends JSStringFormat("iri-reference")

  case object URITemplate extends JSStringFormat("uri-template")

  case object JSONPointer extends JSStringFormat("json-pointer")

  case object Regex extends JSStringFormat("regex")

  val values = findValues
}