import math.Ordering

object mergesort2 {

  /*
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }
  */

  //def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      //merge(msort(fst)(ord), msort(snd)(ord)
      merge(msort(fst), msort(snd))
    }
  }

  val nums = List(-5, 6, 3, 2, 7)
  val fruit = List("apple", "pear", "orange", "pineapple")

  // inferred parameters' type
  //msort(nums)((x, y) => x < y)
  //msort(fruit)((x, y) => x.compareTo(y) < 0)

  msort(nums)(Ordering.Int)
  msort(nums)
  msort(fruit)(Ordering.String)
  msort(fruit)

}