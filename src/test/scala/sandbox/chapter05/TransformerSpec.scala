package sandbox.chapter05

import org.scalatest.{FlatSpec, Matchers}

class TransformerSpec extends FlatSpec with Matchers {

  behavior of "Transformer"

  it should "return a report about necessity of a recharging" in {
    val actual = Transformer.tacticalReport("Jazz", "Bumblebee")
    val expected = "Jazz and Bumblebee need a recharge."

    actual shouldBe expected
  }

  it should "return a report about readiness of rolling out" in {
    val actual = Transformer.tacticalReport("Bumblebee", "Hot Rod")
    val expected = "Bumblebee and Hot Rod are ready to roll out!"

    actual shouldBe expected
  }

  it should "return report about Ironhide unreachability" in {
    val actual = Transformer.tacticalReport("Jazz", "Ironhide")
    val expected = "Comms error: Ironhide is unreachable."

    actual shouldBe expected
  }
}
