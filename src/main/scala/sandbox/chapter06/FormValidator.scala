package sandbox.chapter06

import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.apply._

object FormValidator {

  type AllErrorsOr[A] = Validated[List[String], A]
  type ErrorsOr[A] = Either[List[String], A]


  def getValue(fieldName: String)(data: Map[String, String]): ErrorsOr[String] =
    data.get(fieldName).toRight(List(s"$fieldName field is not specified."))

  def parseInt(fieldName: String)(fieldValue: String): ErrorsOr[Int] =
    Either.catchOnly[NumberFormatException](fieldValue.toInt)
    .leftMap(_ => List(s"$fieldName must be an Integer"))

  def nonBlank(fieldName: String)(fieldValue: String): ErrorsOr[String] =
   fieldValue.asRight.ensure(List(s"$fieldName field cannot be blank"))(_.nonEmpty)

  def nonNegative(fieldName: String)(fieldValue: Int): ErrorsOr[Int] =
    fieldValue.asRight.ensure(List(s"$fieldName field cannot be negative"))(_ >= 0)

  def readName(data: Map[String, String]): ErrorsOr[String] =
    getValue("name")(data)
      .flatMap(nonBlank("name"))

  def readAge(data: Map[String, String]): ErrorsOr[Int] =
    getValue("age")(data)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  def readUser(data: Map[String, String]): AllErrorsOr[User] =
    (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply)
}