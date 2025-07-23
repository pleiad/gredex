package glang.syntax
import Syntax.*

/** This Class represents a hole in a program. Is used to mark which expression
  * is being processed by the elaboration.
  */
case class Hole() extends Term {
  def pprint = "[]"

  def subst(y: Var, x: Var) = this
}

/** This class represents a variable in the language.
  *
  * @param x
  *   The name of the variable.
  */
case class Var(x: String) extends Term {
  def pprint = x.toString

  def subst(y: Var, x2: Var) = if (y.x == x) x2 else this

  def toLatex = x
}
