package sandbox.chapter03

import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter03.Codec._
import sandbox.chapter03.CodecInstances._

class CodecSpec extends FlatSpec with Matchers {

  behavior of "encode"

  it should "encode an Int to String" in {
    encode(42) shouldBe "42"
  }

  it should "encode a Double to String" in {
    encode(42.0) shouldBe "42.0"
  }

  it should "encode a Boolean to String" in {
    encode(true) shouldBe "true"
  }

  it should "encode a Box(Double) to String" in {
    encode(Box(42.0)) shouldBe "42.0"
  }

  behavior of "decode"

  it should "decode a String to Int" in {
    decode[Int]("42") shouldBe 42
  }

  it should "decode a String to Double" in {
    decode[Double]("42.0") shouldBe 42.0
  }

  it should "decode a String to Boolean" in {
    decode[Boolean]("true") shouldBe true
  }

  it should "decode a String to Box[Double]" in {
    decode[Box[Double]]("42.0") shouldBe Box(42.0)
  }
}
