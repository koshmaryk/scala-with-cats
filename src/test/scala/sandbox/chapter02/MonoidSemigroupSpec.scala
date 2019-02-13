package sandbox.chapter02

import org.scalatest.{FlatSpec, Matchers}

class MonoidSemigroupSpec extends FlatSpec with Matchers {

  private val booleans = for {
    a <- Seq(true, false)
    b <- Seq(true, false)
    c <- Seq(true, false)
  } yield (a, b, c)

  behavior of "booleanAndMonoid"

  it should "return true when combining two true values" in {
    Monoid.booleanAndMonoid.combine(true, true) shouldBe true
  }

  it should "return false when combining a true and false value" in {
    Monoid.booleanAndMonoid.combine(true, false) shouldBe false
  }

  it should "return false when combining a false and true value" in {
    Monoid.booleanAndMonoid.combine(false, true) shouldBe false
  }

  it should "return false when combining two false values" in {
    Monoid.booleanAndMonoid.combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import Monoid.booleanAndMonoid

    Seq(true, false) map {
      MonoidLaws.identityLaw(_) shouldBe true
    }
  }

  it should "obey the associative law" in {
    import Monoid.booleanAndMonoid

    booleans map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }

  behavior of "booleanOrMonoid"

  it should "return true when combining two true values" in {
    Monoid.booleanOrMonoid.combine(true, true) shouldBe true
  }

  it should "return true when combining a true and false value" in {
    Monoid.booleanOrMonoid.combine(true, false) shouldBe true
  }

  it should "return true when combining a false and true value" in {
    Monoid.booleanOrMonoid.combine(false, true) shouldBe true
  }

  it should "return false when combining two false values" in {
    Monoid.booleanOrMonoid.combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import Monoid.booleanOrMonoid

    Seq(true, false) map {
      MonoidLaws.identityLaw(_) shouldBe true
    }
  }

  it should "obey the associative law" in {
    import Monoid.booleanOrMonoid

    booleans map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }

  behavior of "booleanEitherMonoid"

  it should "return false when combining two true values" in {
    Monoid.booleanEitherMonoid.combine(true, true) shouldBe false
  }

  it should "return true when combining a true and false value" in {
    Monoid.booleanEitherMonoid.combine(true, false) shouldBe true
  }

  it should "return true when combining a false and true value" in {
    Monoid.booleanEitherMonoid.combine(false, true) shouldBe true
  }

  it should "return false when combining two false values" in {
    Monoid.booleanEitherMonoid.combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import Monoid.booleanEitherMonoid

    Seq(true, false) map {
      MonoidLaws.identityLaw(_) shouldBe true
    }
  }

  it should "obey the associative law" in {
    import Monoid.booleanEitherMonoid

    booleans map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }

  behavior of "booleanXorMonoid"

  it should "return false when combining two true values" in {
    Monoid.booleanXorMonoid.combine(true, true) shouldBe true
  }

  it should "return true when combining a true and false value" in {
    Monoid.booleanXorMonoid.combine(true, false) shouldBe false
  }

  it should "return true when combining a false and true value" in {
    Monoid.booleanXorMonoid.combine(false, true) shouldBe false
  }

  it should "return false when combining two false values" in {
    Monoid.booleanXorMonoid.combine(false, false) shouldBe true
  }

  it should "obey the identity law" in {
    import Monoid.booleanXorMonoid

    Seq(true, false) map {
      MonoidLaws.identityLaw(_) shouldBe true
    }
  }

  it should "obey the associative law" in {
    import Monoid.booleanXorMonoid

    booleans map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }
}
