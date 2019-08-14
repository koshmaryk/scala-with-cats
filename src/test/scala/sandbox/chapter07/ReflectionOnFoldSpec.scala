package sandbox.chapter07

import sandbox.chapter07.ReflectionOnFold._
import org.scalatest.{FlatSpec, Matchers}

class ReflectionOnFoldSpec extends FlatSpec with Matchers {
  val input = List(1, 2, 3)

  behavior of "map"

  it should "return List(2, 3, 4)" in {
    map(input)(_ + 1) shouldBe List(2, 3, 4)
  }

  behavior of "flatMap"

  it should "return List(2, 3, 4)" in {
    flatMap(input)(i => List(i + 1)) shouldBe List(2, 3, 4)
  }


  behavior of "filter"

  it should "return 2" in {
    filter(input)(_ % 2 == 0) shouldBe List(2)
  }

  behavior of "sum"

  it should "return 6" in {
    sum(input) shouldBe 6
  }
}
