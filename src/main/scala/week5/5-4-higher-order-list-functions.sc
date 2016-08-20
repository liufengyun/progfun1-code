object lecture4 {

  //
  // Common patterns on lists: Transforming
  //

  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }

  scaleList(Nil, 2)
  scaleList(List(2, 3, 5), 3)

  // A simple definition of 'map' in the List class:
  /*
  abstract class List[T] {
    ...
    def map[U](f: T => U): List[U] = this match {
      case Nil     => this
      case x :: xs => f(x) :: xs.map(f)
    }
  }
  */

  // scaleList rewritten with map
  def scaleList2(xs: List[Double], factor: Double) = xs map (x => x * factor)

  scaleList2(Nil, 2)
  scaleList2(List(2, 3, 5), 3)

  // Exercise
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

  def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

  squareList(List(2, 3, 5))
  squareList2(List(2, 3, 5))

  //
  // Common patterns on lists: Filtering
  //

  val nums = List(2, -4, 5, 7, 1)

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }

  posElems(nums)

  // Filtering pattern generalized in the List class:
  /*
  abstract class List[T] {
    ...
    def filter(p: T => Boolean): List[T] = this match {
      case Nil     => this
      case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
    }
  }
  */

  def posElems2(xs: List[Int]): List[Int] = xs filter (x => x > 0)

  posElems2(nums)

}