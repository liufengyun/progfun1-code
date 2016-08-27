package week4

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = {
    def parens(e: Expr) = e match {
      case Sum(_, _) => "(" + e.show + ")"
      case _ => e.show
    }
    this match {
      case Number(n) => n.toString
      case Sum(l, r) => l.show + " + " + r.show
      case Prod(l, r) => parens(l) + " * " + parens(r)
      case Var(x) => x
    }
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(x: String) extends Expr

