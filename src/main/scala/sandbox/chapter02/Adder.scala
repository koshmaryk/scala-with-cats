package sandbox.chapter02

import cats.syntax.semigroup._

object Adder {
  def add[A: cats.Monoid](items: List[A]): A = // [A: Monoid] scala's context bound
    items.foldLeft(cats.Monoid[A].empty)(_ |+| _) // items.foldLeft(0)(_ + _) or items.sum
}
