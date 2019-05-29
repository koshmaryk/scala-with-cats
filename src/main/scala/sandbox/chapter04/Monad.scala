package sandbox.chapter04

import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](x: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(x => pure(func(x)))
}