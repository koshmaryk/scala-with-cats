package sandbox.chapter03

import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter01.PrintableSyntax._
import sandbox.chapter01.PrintableInstances._

class PrintableSpec extends FlatSpec with Matchers {

  behavior of "format"

  it should "return a formatted string" in {
    1.format shouldBe "1"
  }

  it should "return a formatted boolean" in {
    true.format shouldBe "yes"
  }

  it should "return a formatted box" in {
    Box("Foo").format shouldBe "Foo"
  }
}

