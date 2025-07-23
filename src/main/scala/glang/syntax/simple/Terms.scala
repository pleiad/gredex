package glang.syntax.simple
import glang.syntax.{Syntax, Type, Var}
import Syntax.*

/** Values! */

/** Case class representing an integer value.
  *
  * @param v
  *   The integer value.
  */
case class Number(v: Int) extends Term {
  def pprint = v.toString
}

/** Case class representing a boolean value.
  * @param b
  *   The boolean value.
  */
case class Bool(b: Boolean) extends Term {
  def pprint = b.toString
}

/** Case class representing a unit value.
  */
case class UnitVal() extends Term {
  def pprint = "()"
}

/** Class representing a lambda abstraction
  *
  * @param x
  *   The variable to bind the lambda to.
  * @param t
  *   The term to bind the lambda to.
  */
case class Lambda(x: Var, ty: Type, t: Term) extends Term {
  def pprint = s"(${lambdaS} ${x.pprint}: ${ty.pprint}. ${t.pprint})"
}

/** Redexes */

/** Class representing a binary operation (t1 -> t2 -> t3)
  * @param t1
  *   The first term in the operation.
  * @param t2
  *   The second term in the operation.
  * @param op
  *   The operation to perform.
  */
case class BinOp(t1: Term, t2: Term, op: String) extends Term {
  def pprint = t1.pprint + " " + op + " " + t2.pprint
}

/** Class representing a unary operation (t1 -> t2)
  * @param t
  *   The term to apply the operation to.
  * @param op
  *   The operation to perform.
  */
case class UnOp(t: Term, op: String) extends Term {
  def pprint = op + " " + t.pprint
}

/** Class representing a pair of terms
  * @param t1
  *   The first term in the pair.
  * @param t2
  *   The second term in the pair.
  */
case class Pair(t1: Term, t2: Term) extends Term {
  def pprint = "<" + t1.pprint + ", " + t2.pprint + ">"
}

/** Class Fix represents a fixpoint operator
  * @param x
  *   The variable to bind the fixpoint to.
  * @param t
  *   The term to bind the fixpoint to.
  */
case class Fix(x: Var, ty: Type, t: Term) extends Term {
  def pprint = "(fix " + x.pprint + ": " + ty + ". " + t.pprint + ")"
}

/** Class representing an if-then-else operation
  * @param t1
  *   The condition.
  * @param t2
  *   The then branch.
  * @param t3
  *   The else branch.
  */
case class Ite(t1: Term, t2: Term, t3: Term) extends Term {
  def pprint = "if " + t1.pprint + " then " + t2.pprint + " else " + t3.pprint
}

/** Class representing an application of a term to another term
  * @param t1
  *   The function to apply.
  * @param t2
  *   The argument to apply the function to.
  */
case class App(t1: Term, t2: Term) extends Term {
  def pprint = t1.pprint + " " + t2.pprint
}

/** Class representing a type ascription
  * @param e
  *   The term to ascribe the type to.
  * @param t
  *   The type to ascribe to the term.
  */
case class Asc(e: Term, t: Type) extends Term {
  def pprint = e.pprint + " :: " + t.pprint
}

/** Class representing a let binding
  * @param x
  *   The variable to bind the term to.
  * @param t1
  *   The term to bind to the variable.
  * @param t2
  *   The term to evaluate with the variable bound.
  */
case class Let(x: Var, t1: Term, t2: Term) extends Term {
  def pprint = s"let ${x.pprint} = ${t1.pprint} in ${t2.pprint}"
}

/** Class representing the projection of the first element of a pair
  * @param t
  *   The term to evaluate.
  */
case class First(t: Term) extends Term {
  def pprint = "fst(" + t.pprint + ")"
}

/** Class representing the projection of the second element of a pair
  * @param t
  *   The term to evaluate.
  */
case class Second(t: Term) extends Term {
  def pprint = "snd(" + t.pprint + ")"
}

/** Class representing the left injection of a term into a sum type
  * @param t
  *   The term to inject.
  * @param tpe
  *   The type of the sum type.
  */
case class Inl(t: Term, tpe: Type) extends Term {
  def pprint = "inj1(" + t.pprint + ")"
}

/** Class representing the right injection of a term into a sum type
  * @param t
  *   The term to inject.
  * @param tpe
  *   The type of the sum type.
  */
case class Inr(t: Term, tpe: Type) extends Term {
  def pprint = "inj2(" + t.pprint + ")"
}

/** Class representing a case expression
  * @param t
  *   The term to evaluate.
  * @param x1
  *   The variable to bind the left branch to.
  * @param t1
  *   The term to evaluate if the left branch is taken.
  * @param x2
  *   The variable to bind the right branch to.
  * @param t2
  *   The term to evaluate if the right branch is taken.
  */
case class Case(t: Term, x1: Var, t1: Term, x2: Var, t2: Term) extends Term {
  def pprint =
    s"case ${t.pprint} of {${x1.pprint} => ${t1.pprint}} {${x2.pprint} => ${t2.pprint}}"
}
