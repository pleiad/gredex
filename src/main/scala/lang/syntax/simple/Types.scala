package lang.syntax.simple

import lang.syntax.Syntax.SimpleType
import lang.syntax.Type
import TypeOps.*

/** Case class representing an integer type. An int type is a simple type and is
  * not unknown
  */
case class IntType() extends Type with SimpleType {
  def pprint = "int"

  def toLatex = "\\mathsf{int}"

}

/** Case class representing a boolean type. A bool type is a simple type and is
  * not unknown
  */
case class BoolType() extends Type with SimpleType {
  def pprint = "bool"

  def toLatex = "\\mathsf{bool}"

}

/** Case class representing an unknown type.
  */
case class Unknown() extends Type {
  def pprint = "*"

  def toLatex = "\\star"

}

/** Case class representing a unit type. A unit type is a simple type and is not
  * unknown
  */
case class UnitType() extends Type with SimpleType {
  def pprint = "Unit"

  def toLatex = "\\mathsf{unit}"
}

/** Case class representing a function type. We use parentify to add parenthesis
  * to the function type.
  *
  * @param t1
  *   The domain of the function.
  * @param t2
  *   The codomain of the function.
  */
case class FuncType(t1: Type, t2: Type) extends Type {

  def pprint = t1.pprint + " -> " + parentify(t2, _.pprint)

  def toLatex = s"${t1.toLatex} \\to ${parentify(t2, _.toLatex)}"

}

/** Case class representing a sum type. We use parentify to add parenthesis to
  * the sum type.
  *
  * @param t1
  *   The first type in the sum.
  * @param t2
  *   The second type in the sum.
  */
case class SumType(t1: Type, t2: Type) extends Type {

  def pprint = parentify(t1, _.pprint) + " + " + parentify(t2, _.pprint)

  def toLatex = parentify(t1, _.toLatex) + s" + " + parentify(t2, _.toLatex)
}

/** Case class representing a pair type. We use parentify to add parenthesis to
  * the pair type.
  *
  * @param t1
  *   The first type in the pair.
  * @param t2
  *   The second type in the pair.
  */
case class PairType(t1: Type, t2: Type) extends Type {

  def pprint = parentify(t1, _.pprint) + " x " + parentify(t2, _.pprint)

  def toLatex =
    parentify(t1, _.toLatex) + s" \\times  " + parentify(t2, _.toLatex)

}
