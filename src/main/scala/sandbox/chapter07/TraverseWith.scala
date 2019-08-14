package sandbox.chapter07

import cats.Applicative
import cats.data.Validated
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.instances.option._
import cats.instances.list._

object TraverseWith {

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  def processToOption(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]
  def processToValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }
}
