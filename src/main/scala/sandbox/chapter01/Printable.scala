package sandbox.chapter01

import sandbox.chapter03.Box

trait Printable[A] { self => /* private[this] val self = this, an alias for this */
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = // (value: B) => self.format(func(value))
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String = value
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
}

/* Interface Object */
object Printable {
  def apply[A](implicit instance: Printable[A]): Printable[A] = instance

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
}

/* Interface Syntax */
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = Printable.format(value)

    def print(implicit p: Printable[A]): Unit = Printable.print(value)
  }
}