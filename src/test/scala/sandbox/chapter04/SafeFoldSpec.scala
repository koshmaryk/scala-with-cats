package sandbox.chapter04

import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter04.SafeFold._

class SafeFoldSpec extends FlatSpec with Matchers {

  behavior of "foldRight"

  it should "fold over a small list" in {
    foldRight(List(1, 2, 3), 0)((i, acc) => i + acc) shouldBe 6
  }

  it should "fold over a larger list without triggering a StackOverflowError" in {
    foldRight(List.fill(1000000)(1), 0)((i, acc) => i + acc) shouldBe 1000000
  }
}
