package com.uralian.jason.v7.ast

import com.uralian.jason.util.JsonUtils
import org.json4s.FieldSerializer.renameFrom
import org.json4s.{FieldSerializer, JArray, JValue}

/**
 * JSON Schema type annotation.
 *
 * @param title       type name.
 * @param description type description.
 * @param default     default value.
 * @param examples    example values.
 * @param readOnly    whether the value can be only read (eg. in an API request).
 * @param writeOnly   whether the value can be only written (eg. in an API request).
 */
final case class Annotation private(title: Option[String] = None,
                                    description: Option[String] = None,
                                    default: Option[JValue] = None,
                                    examples: Option[JArray] = None,
                                    comment: Option[String] = None,
                                    readOnly: Option[Boolean] = None,
                                    writeOnly: Option[Boolean] = None)

/**
 * Factory for [[Annotation]] instances.
 */
object Annotation extends JsonUtils {
  /**
   * JSON serializer for [[Annotation]] instances.
   */
  val serializer = FieldSerializer[Annotation](
    deserializer = renameFrom("$comment", "comment")
  )

  /**
   * Checks if the [[Annotation]] instance contains any information.
   *
   * @param annotation annotation to check.
   * @return `Some(annotation)` if the annotation is not empty, `None` otherwise.
   */
  def noneIfEmpty(annotation: Option[Annotation]): Option[Annotation] = {
    annotation.filter(a =>
      a.title.isDefined || a.description.isDefined || a.comment.isDefined || a.examples.isDefined ||
        a.default.isDefined || a.readOnly.isDefined || a.writeOnly.isDefined
    )
  }
}