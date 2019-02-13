package sandbox.chapter02

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid: cats.Monoid[Order] =
    new cats.Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(o1: Order, o2: Order): Order =
        Order(
          o1.totalCost + o2.totalCost,
          o1.quantity + o2.quantity
        )
    }
}
