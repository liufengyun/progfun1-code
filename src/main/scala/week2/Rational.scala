package week2

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  /**
    * Another constructor, overloading the default
    **/
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def +(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def unary_- = new Rational(-this.numer, this.denom)

  def -(that: Rational) = this + -that

  def *(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def /(that: Rational) = this * new Rational(that.denom, that.numer)

  override def toString = this.numer + "/" + this.denom
}