package sandbox.chapter06

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter06.FormValidator._

class FormValidatorSpec extends FlatSpec with Matchers {

  val data: Map[String, String] = Map("name" -> "Bumblebee", "age" -> "21")

  behavior of "getValue()"

  it should "lead to right if value exists" in {
    getValue("name")(data) shouldBe Right("Bumblebee")
  }

  it should "lead to left if value not exists" in {
    getValue("birth")(data) shouldBe Left(List("birth field is not specified."))
  }

  behavior of "parseInt()"

  it should "lead to right when parsing was successful" in {
    parseInt("age")("21") shouldBe Right(21)
  }

  it should "lead to left when error occur during parsing" in {
    parseInt("age")("string") shouldBe Left(List("age must be an Integer"))
  }

  behavior of "nonBlank()"

  it should "lead to right if name was proper " in {
   nonBlank("name")("Bumblebee") shouldBe Right("Bumblebee")
  }

  it should "lead to left if name was wrong" in {
    nonBlank("name")("") shouldBe Left(List("name field cannot be blank"))
  }

  behavior of "nonNegative()"

  it should "lead to right if age was proper" in {
    nonNegative("age")(21) shouldBe Right(21)
  }

  it should "lead to left if age was wrong" in {
    nonNegative("age")(-21) shouldBe Left(List("age field cannot be negative"))
  }

  behavior of "readUser()"

  it should "lead to valid user" in {
    readUser(data) shouldBe Valid(User("Bumblebee", 21))
  }

  it should "lead to multiple error messages during applying invalid user data" in {
    readUser(Map("name" -> "", "age" -> "-21")) shouldBe Invalid(List("name field cannot be blank", "age field cannot be negative"))
  }
}
