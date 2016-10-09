package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      namedExpressions.map { case (name, sigExpr) =>
          (name, Signal(eval(sigExpr(), namedExpressions)))
      }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
      case Plus(a:Expr, b:Expr)   => eval(a, references) + eval(b, references)
      case Minus(a:Expr, b:Expr)  => eval(a, references) - eval(b, references)
      case Times(a:Expr, b:Expr)  => eval(a, references) * eval(b, references)
      case Divide(a:Expr, b:Expr) => eval(a, references) / eval(b, references)
      case Literal(value: Double) => value
      case Ref(name) => eval(getReferenceExpr(name, references), references - name)
  }

  /* Get the Expr for a referenced variables, returns a NaN for unknown var. */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {Literal(Double.NaN) } { exprSignal => exprSignal() }
  }
}
