package sandbox.chapter04

import org.scalatest.{FlatSpec, Matchers}

import sandbox.chapter04.TreeMonad.treeMonad
import sandbox.chapter03.{Branch, Leaf, Tree}

class TreeMonadSpec extends FlatSpec with Matchers {

  def doubleLeaf[A](a: A): Tree[A] =
    Branch(Leaf(a), Leaf(a))

  val someLeaf: Tree[Int] = Leaf(1)
  val someBranch: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  behavior of "TreeMonad"

  it should "work on Leaf" in {
    val expected = Branch(Leaf(1), Leaf(1))
    val actual = treeMonad.flatMap(someLeaf)(doubleLeaf)
    actual shouldBe expected
  }

  it should "work on Branch" in {
    val expected = Branch(Branch(Leaf(1), Leaf(1)), Branch(Branch(Leaf(2), Leaf(2)), Branch(Leaf(3), Leaf(3))))
    val actual = treeMonad.flatMap(someBranch)(doubleLeaf)
    actual shouldBe expected
  }

  it should "have Functor-like behaviour" in {
    val expected = Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
    val actual = treeMonad.map(someBranch)(_ + 1)
    actual shouldBe expected
  }

  it should "support for-comprehensions when the monad is in the implicit scope" in {
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    implicit val implicitTreeMonad: cats.Monad[Tree] = treeMonad

    val xy = for {
      x <- someLeaf
      y <- someBranch
    } yield x + y

    xy shouldBe Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
  }
}
