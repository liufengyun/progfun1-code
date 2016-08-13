import java.lang.Math._

import scala.annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

sum(x => x)(1, 10)

def sumInts: (Int, Int) => Int = sum(x => x)
def sumCubes: (Int, Int) => Int = sum(x => x * x * x)
sumInts(1, 10)
sumCubes(1, 10)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) * acc)
  }
  loop(a, 1)
}

def productInts: (Int, Int) => Int = product(x => x)
def productCubes: (Int, Int) => Int = product(x => x * x * x)
productInts(1, 10)
productCubes(1, 10)

def factorial(n: Int): Int = product(x => x)(1, n)
factorial(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, combine(f(a), acc))
  }
  loop(a, zero)
}

def su: (Int, Int) => Int = mapReduce(x => x, (x, y) => x + y, 0)
su(1, 5)

def prod: (Int, Int) => Int = mapReduce(x => x, (x, y) => x * y, 1)
prod(1, 5)

def fact(n: Int): Int = prod(1, n)
fact(5)

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double): Boolean = abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x / 2)(1.0)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
sqrt(4)
sqrt(2)
