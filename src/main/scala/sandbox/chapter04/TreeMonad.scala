package sandbox.chapter04

import sandbox.chapter03.{Branch, Leaf, Tree}

object TreeMonad {
  val treeMonad: cats.Monad[Tree] = new cats.Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(value) => f(value)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Leaf(Left(value)) => tailRecM(value)(f)
        case Leaf(Right(value)) => Leaf(value)
        case Branch(left, right) =>
          Branch(
            flatMap(left) {
              case Left(l) => tailRecM(l)(f)
              case Right(l) => pure(l)
            },
            flatMap(right) {
              case Left(r) => tailRecM(r)(f)
              case Right(r) => pure(r)
            }
          )
      }
    }
  }
}
