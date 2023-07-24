package com.uralian.jason

import org.mockito.ArgumentCaptor
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec._
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.reflect.ClassTag

/**
 * Base trait for unit test specifications.
 */
abstract class AbstractUnitSpec extends AnyWordSpec
  with BeforeAndAfterAll
  with Matchers
  with OptionValues
  with ScalaFutures
  with Inside
  with MockitoSugar
  with ScalaCheckPropertyChecks {

  /**
   * Creates a mockito argument captor for the given class.
   *
   * @param tag
   * @tparam T
   * @return
   */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  protected def argumentCaptor[T](implicit tag: ClassTag[T]) =
    ArgumentCaptor.forClass[T, T](tag.runtimeClass.asInstanceOf[Class[T]])
}