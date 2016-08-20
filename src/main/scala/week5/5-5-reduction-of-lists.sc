object reduction {

  val nums = List(1, 2, 3, 4, 5)

  // sum(List(x1, ..., xn))     = 0 + x1 + ... + xn

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }

  sum(nums)

  // product(List(x1, ..., xn)) = 1 * x1 * ... * xn

  def product(xs: List[Int]): Int = xs match {
    case Nil => 1
    case y :: ys => y * product(ys)
  }

  product(nums)

  //
  // ReduceLeft
  //
  // List(x1, ..., xn) reduceLeft op = (...(x1 op x2) op ... ) op xn

  def sumRL(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)

  def productRL(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

  // (_ op _) is the shorter form of ((x, y) => x op y)

  //
  // FoldLeft
  //
  // (List(x1, ..., xn) foldLeft z)(op) = (...(z op x1) op ... ) op xn

  def sumFL(xs: List[Int]) = (xs foldLeft 0) (_ + _)

  def productFL(xs: List[Int]) = (xs foldLeft 1) (_ * _)

  // A possible implementation in the List class:
  /*
  abstract class List[T] {
    def reduceLeft(op: (T, T) => T): T = this match {
      case Nil     => throw new Error("Nil.reduceLeft")
      case x :: xs => (xs foldLeft x)(op)
    }
    def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
      case Nil     => z
      case x :: xs => (xs foldLeft op(z, x))(op)
    }
  }
  */

  //
  // ReduceRight and FoldRight
  //
  // List(x1, ..., x{n-1}, xn) reduceRight op = x1 op ( ... (x{n-1} op xn) ... )
  // (List(x1, ..., xn) foldRight acc)(op)    = x1 op ( ... (xn op acc) ... )

  /*
  def reduceRight(op: (T, T) => T): T = this match {
    case Nil      => throw new Error("Nil.reduceRight")
    case x :: Nil => x
    case x :: xs  => op(x, xs.reduceRight(op))
  }
  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
    case Nil     => z
    case x :: xs => op(x, (xs foldRight z)(op))
  }
  */

  // Exercise - reformulation of concat with foldRight

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  // It's possible to replace foldRight by foldLeft? No, because the types would not work out

  //def concatFL[T](xs: List[T], ys: List[T]): List[T] =
  //  (xs foldLeft ys)(_ :: _)

  // Reversing lists with foldLeft (note that the operands are swapped):
  // what is the complexity of this implementation of revers?
  def reverse[T](xs: List[T]): List[T] = (xs foldLeft List[T]()) ((acc, x) => x :: acc)

  reverse(nums)

  //
  // Exercises
  //

  def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldRight List[U]()) (???)

  def lengthFun[T](xs: List[T]): Int = (xs foldRight 0) (???)

}