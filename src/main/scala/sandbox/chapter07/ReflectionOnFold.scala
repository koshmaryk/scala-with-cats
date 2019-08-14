package sandbox.chapter07

object ReflectionOnFold {

  /*
    List(1, 2, 3).foldLeft(List.empty[Int])((acc, i) => i :: acc)
    List(1, 2, 3).foldRight(List.empty[Int])((i, acc) => i :: acc)
  */

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B])((item, acc) => func(item) :: acc)

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((item, acc) => func(item) ::: acc)

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A])((item, acc) => if (func(item)) {
      item :: acc
    } else {
      acc
    })

  def sum(list: List[Int]): Int =
    list.foldRight(0)(_ + _)
}
