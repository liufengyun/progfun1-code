package week5

object lecture1 {

  // Implementation of 'last' (slides 3-7)
  def last[T](xs: List[T]): T = xs match {
    case List()  => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  // Exercise: Implementation of 'init' (slides 8-9)
  def init[T](xs: List[T]): List[T] = xs match {
    case List()  => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  // Implementation of 'concat' (slides 10-14)
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List()  => ys
    case z :: zs => z :: concat(zs, ys)
  }

  // Implementation of 'reverse' (slides 15-17)
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List()  => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  //
  // Exercises
  //

  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  // Exercise
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

  removeAt(1, nums)
  removeAt(1, List("a", "b", "c", "d"))

  // Exercise (harder, optional)
  def flatten(xs: List[Any]): List[Any] = ???

  flatten(diag3)
  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

}