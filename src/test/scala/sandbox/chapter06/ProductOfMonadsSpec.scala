package sandbox.chapter06

import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter06.ProductOfMonads.product

import cats.instances.option._
import cats.instances.list._

class ProductOfMonadsSpec extends FlatSpec with Matchers {

  behavior of "product()"

  it should "successfully combine context of two Options" in {
    product(Option(1), Option(2)) shouldBe Option((1, 2))
  }

  it should "successfully combine context of two Lists" in {
    product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }
}
