package sandbox.chapter06

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

object ProductOfMonads {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)

}
