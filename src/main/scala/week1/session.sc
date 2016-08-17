import scala.annotation.tailrec

object session {

  def not(x: Boolean): Boolean = if (x) false else true
  def and(x: Boolean, y: => Boolean): Boolean = if (x) y else false
  def or(x: Boolean, y: => Boolean): Boolean = if (x) true else y

  not(true)
  not(false)

  and(true, true)
  and(true, false)
  and(false, true)
  and(false, false)

  or(true, true)
  or(true, false)
  or(false, true)
  or(false, false)

  def loop: Boolean = loop

  // these functions calls will terminate
  // because the second parameter is called by name
  and(false, loop)
  or(true, loop)

  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n == 0) acc
      else loop(n - 1, n * acc)
    loop(n, 1)
  }

  factorial(5)

}