package week4

// Peano numbers
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true

  def predecessor = throw new Error("Zero.predecessor")

  def +(that: Nat) = that

  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")

  override def toString = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def predecessor = n

  def +(that: Nat) = new Succ(n + that)

  def -(that: Nat) = if (that.isZero) this else n - that.predecessor

  override def toString = "1+" + n
}
