package sandbox.chapter07

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import sandbox.chapter07.TraverseWith._
import org.scalatest.{FlatSpec, Matchers}

class TraverseWithSpec extends FlatSpec with Matchers {

  behavior of "Traversing with Vectors"

  it should "combine elements from each of 2 vector" in {
    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  }

  it should "combine elements from each of 3 vector" in {
    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe
      Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
  }


  behavior of "Traversing with Options"

  it should "produce empty Option" in {
    processToOption(List(1, 2, 3)) shouldBe Option.empty
  }

  it should "return the same List wrapped by Option" in {
    processToOption(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
  }

  it should "return empty Option" in {
    processToOption(List(1, 2, 3)) shouldBe None
  }


  behavior of "Traversing with Validated"

  it should "return the same List wrapped by Valid" in {
    processToValidated(List(2, 4, 6)) shouldBe Valid(List(2, 4, 6))
  }

  it should "return Invalid with List of errors inside" in {
    processToValidated(List(1, 2, 3)) shouldBe Invalid(List("1 is not even", "3 is not even"))
  }
}
