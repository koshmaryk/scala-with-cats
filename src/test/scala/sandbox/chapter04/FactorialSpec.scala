package sandbox.chapter04

import cats.data.Writer
import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter04.Factorial._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FactorialSpec extends FlatSpec with Matchers {

  behavior of "factorial"

  it should "capture separate logs messages in a Writer" in {
    val writers = Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)

    writers shouldBe Vector(
      Writer(Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6),
      Writer(Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
    )
  }
}
