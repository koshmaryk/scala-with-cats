package sandbox.chapter02

object SetInstances {
  implicit def setUnion[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def setIntersection[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

  implicit def symmetricDiff[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] =
        (x diff y) union (y diff x)
      def empty: Set[A] = Set.empty
    }
}
