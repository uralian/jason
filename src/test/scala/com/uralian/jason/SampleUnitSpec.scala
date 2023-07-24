package com.uralian.jason

import com.uralian.jason.model.SchemaDraft
import org.mockito.ArgumentMatchers.anyInt
import org.mockito.Mockito.{times, verify, when}
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalacheck.Gen

/**
 * Sample unit specifications.
 */
abstract class SampleUnitSpec extends AbstractUnitSpec {

  "simple tests" should {
    "work for matchers" in {
      val v3 = SchemaDraft.V3
      v3.entryName must not be empty
      v3.url.size must be > 10
      v3.description startsWith "Original" mustBe true
    }
    "work for exceptions" in {
      val v3 = SchemaDraft.V3
      noException should be thrownBy v3.entryName == "v3"
    }
  }

  "mockito tests" should {
    "work for simple verifications" in {
      val m = mock[List[Int]]
      val order = Mockito.inOrder(m)
      val _ = m.headOption
      val _ = m.slice(1, 3)
      val _ = m.slice(2, 4)
      order.verify(m, Mockito.atLeast(1)).headOption
      order.verify(m, times(2)).slice(anyInt, anyInt)
    }
    "work for argument capturing" in {
      val m = mock[List[String]]
      val arg1 = argumentCaptor[Int]
      val _ = m.slice(5, 10)
      verify(m).slice(arg1.capture(), anyInt)
      arg1.getValue mustBe 5
    }
    "work for stubbing" in {
      val m = mock[List[Int]]
      when(m.slice(ArgumentMatchers.eq(1), anyInt)).thenReturn(List(1, 1))
      when(m.slice(ArgumentMatchers.eq(2), anyInt)).thenReturn(List(2, 2))
      m.slice(1, 5) mustBe List(1, 1)
      m.slice(2, 5) mustBe List(2, 2)
    }
  }

  "property check tests" should {
    "work for simple properties" in {
      val f = (x: BigInt, y: BigInt) => x / y
      forAll("x", "y") { (x: BigInt, y: BigInt) =>
        whenever(x != 0 && y != 0) {
          if ((x > 0 && y > 0) || (x < 0 && y < 0))
            f(x, y) must be >= BigInt(0)
          else
            f(x, y) must be <= BigInt(0)
        }
      }
    }
    "work for tables" in {
      val f = (x: BigInt, y: BigInt) => x / y
      val invalid = Table(
        ("x", "y"),
        (BigInt(0), BigInt(0)),
        (BigInt(1), BigInt(0)),
        (BigInt(-1), BigInt(0))
      )
      forAll(invalid) { (x: BigInt, y: BigInt) =>
        an[Exception] must be thrownBy f(x, y)
      }
    }
    "work for custom generators" in {
      case class Point(id: String, x: Int, y: Int) {
        def distanceTo(that: Point) = {
          val dx = this.x.toDouble - that.x.toDouble
          val dy = this.y.toDouble - that.y.toDouble
          Math.sqrt(dx * dx + dy * dy)
        }
      }
      val ids = Gen.identifier.map(_.take(10))
      val coords = Gen.choose(0, 100)
      val points = for {
        id <- ids
        x <- coords
        y <- coords
      } yield Point(id, x, y)
      forAll((points, "p1"), (points, "p2")) { (p1: Point, p2: Point) =>
        p1.distanceTo(p2) mustBe p2.distanceTo(p1)
      }
    }
  }

  override protected def beforeAll(): Unit = {}

  override protected def afterAll(): Unit = {}
}