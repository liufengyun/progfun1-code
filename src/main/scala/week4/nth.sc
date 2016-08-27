import week4._

object nth {
  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)

  val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
  nth(2, list)

  val list1 = List.apply(1, 2)
  val list2 = List(1, 2)

  val nil = Zero
  val one = nil.successor
  val two = one + one
  val thr = two + one
  val fou = two + two
  val fiv = two + thr
  val dif = fiv - thr

  //  val x = Number(5).eval //> x  : Int = 5
}