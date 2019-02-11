package sandbox.chapter01

import cats.{Show, Eq}
import org.scalatest.{FlatSpec, Matchers}

class CatSpec extends FlatSpec with Matchers {

  behavior of "cats.Show, cats.Eq, Printable"

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  it should "provide proper textual representation of Cat through Printable Interface Object" in {
    import sandbox.chapter01.PrintableInstances._

    val printableCat: Printable[Cat] = Printable.apply[Cat]

    printableCat.format(cat1) shouldBe "Garfield is a 38 year-old orange and black cat."
    printableCat.format(cat2) shouldBe "Heathcliff is a 33 year-old orange and black cat."
  }

  it should "provide proper textual representation of Cat through Printable Interface Syntax" in {
    import sandbox.chapter01.PrintableInstances._
    import sandbox.chapter01.PrintableSyntax._

    cat1.format shouldBe "Garfield is a 38 year-old orange and black cat."
    cat2.format shouldBe "Heathcliff is a 33 year-old orange and black cat."
  }

  it should "provide proper textual representation of Cat through Show" in {
    val showCat: Show[Cat] = Show.apply[Cat]
    showCat.show(cat1) shouldBe "Garfield is a 38 year-old orange and black cat."
    showCat.show(cat2) shouldBe "Heathcliff is a 33 year-old orange and black cat."
  }

  it should "determine equality between 2 instances of Cat through Eq" in {
    val eqCat: Eq[Cat] = Eq[Cat]
    assert(eqCat.neqv(cat1, cat2))
    assert(eqCat.eqv(cat1, cat1))
  }
}

