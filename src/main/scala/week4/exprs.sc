import week4._

object exprs {
  Sum(Number(5), Number(9)).show
  Sum(Prod(Number(2), Var("x")), Var("y")).show
  Prod(Sum(Number(2), Var("x")), Var("y")).show

  true match {
    case true =>
      val x = 3
      val y = 5
      x * y
    case false => 10
  }
}
