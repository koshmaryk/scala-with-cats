package sandbox.chapter02

import org.scalatest.{FlatSpec, Matchers}

class SetInstancesSpec extends FlatSpec with Matchers {

  behavior of "setUnion"

  it should "return the union when combining two sets" in {
    SetInstances.setUnion[Int].combine(Set(1, 2, 3), Set(2, 3, 4)) shouldBe Set(1, 2, 3, 4)
  }

  it should "obey the identity law" in {
    import SetInstances.setUnion

    MonoidLaws.identityLaw(Set(1, 2, 3)) shouldBe true
  }

  it should "obey the associative law" in {
    import SetInstances.setUnion

    MonoidLaws.associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)) shouldBe true
  }

  behavior of "setIntersection"

  it should "return the intersection when combining two sets" in {
    SetInstances.setIntersection[Int].combine(Set(1, 2, 3), Set(2, 3, 4)) shouldBe Set(2, 3)
  }

  behavior of "symmetricDiff"

  it should "return the intersection when combining two sets" in {
    SetInstances.symmetricDiff[Int].combine(Set(1, 2, 3), Set(2, 3, 4)) shouldBe Set(1, 4)
  }
}
