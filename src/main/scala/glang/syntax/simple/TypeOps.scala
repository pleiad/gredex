package glang.syntax.simple

import glang.syntax.{HoleType, Type, TypeError}
import glang.syntax.simple.*
import glang.syntax.Syntax
object TypeOps {

  /** Method to add parenthesis to types for printing purposes.
    *
    * @param t
    *   The type to add parenthesis to.
    * @param f
    *   The function to apply to the type.
    * @return
    *   The type with parenthesis added.
    */
  def parentify(t: Type, f: Type => String) = t match {
    case _: FuncType => "(" + f(t) + ")"
    case _: SumType  => "(" + f(t) + ")"
    case _: PairType => "(" + f(t) + ")"
    case _           => f(t)
  }

  /** helper functions over types */

  /** Method to compute the codomain of a function type.
    *
    * @param t
    *   The type to compute the codomain of.
    * @return
    *   The codomain of the function type.
    */
  def cod(t: Type): Type = t match {
    case FuncType(t1, t2) => t2
    case Unknown()        => Unknown()
    case HoleType()       => HoleType()
    case _ =>
      throw new TypeError("Error: \\(" + t.toLatex + "\\) is not a function")
  }

  /** Method to compute the domain of a function type.
    *
    * @param t
    *   The type to compute the domain of.
    * @return
    *   The domain of the function type.
    */
  def dom(t: Type): Type = t match {
    case FuncType(t1, t2) => t1
    case Unknown()        => Unknown()
    case HoleType()       => HoleType()
    case _ =>
      throw new TypeError("Error: \\(" + t.toLatex + "\\) is not a function")
  }

  /** Method to project the first component of a pair type.
    *
    * @param t
    *   The type to project from.
    * @return
    *   The first component of the pair type.
    */
  def pi1(t: Type): Type = t match {
    case PairType(t1, t2) => t1
    case Unknown()        => Unknown()
    case HoleType()       => HoleType()
    case _                => throw new TypeError("Wrong type for pi1 " + t)
  }

  /** Method to project the second component of a pair type.
    *
    * @param t
    *   The type to project from.
    * @return
    *   The second component of the pair type.
    */
  def pi2(t: Type): Type = t match {
    case PairType(t1, t2) => t2
    case Unknown()        => Unknown()
    case HoleType()       => HoleType()
    case _                => throw new TypeError("Wrong type for pi1 " + t)
  }

  /** Method to project the first component of a sum type.
    *
    * @param t
    *   The type to project from.
    * @return
    *   The first component of the sum type.
    */
  def pis1(t: Type): Type = t match {
    case SumType(t1, t2) => t1
    case Unknown()       => Unknown()
    case HoleType()      => HoleType()
    case _               => throw new TypeError("Wrong type for pis1 " + t)
  }

  /** Method to project the second component of a sum type.
    *
    * @param t
    *   The type to project from.
    * @return
    *   The second component of the sum type.
    */
  def pis2(t: Type): Type = t match {
    case SumType(t1, t2) => t2
    case Unknown()       => Unknown()
    case HoleType()      => HoleType()
    case _               => throw new TypeError("Wrong type for pis1 " + t)
  }

  def typeOf(op: String): FuncType = op match {
    case "+"  => FuncType(IntType(), FuncType(IntType(), IntType()))
    case "-"  => FuncType(IntType(), FuncType(IntType(), IntType()))
    case "*"  => FuncType(IntType(), FuncType(IntType(), IntType()))
    case "/"  => FuncType(IntType(), FuncType(IntType(), IntType()))
    case "==" => FuncType(IntType(), FuncType(IntType(), BoolType()))
    case "<"  => FuncType(IntType(), FuncType(IntType(), BoolType()))
    case ">"  => FuncType(IntType(), FuncType(IntType(), BoolType()))
    case Syntax.andS.parser =>
      FuncType(BoolType(), FuncType(BoolType(), BoolType()))
    case Syntax.orS.parser =>
      FuncType(BoolType(), FuncType(BoolType(), BoolType()))
    case Syntax.notS.parser | "not" => FuncType(BoolType(), BoolType())
    case _ => throw new TypeError("Unknown operator: " + op)
  }

  def consistent(t1: Type, t2: Type): Boolean = {
    (t1, t2) match {
      case (_, Unknown())  => true
      case (Unknown(), _)  => true
      case (HoleType(), _) => true
      case (_, HoleType()) => true
      case (FuncType(t11, t12), FuncType(t21, t22)) =>
        consistent(t21, t11) && consistent(t12, t22)
      case (SumType(t11, t12), SumType(t21, t22)) =>
        (consistent(t11, t21) && consistent(t12, t22))
      case (PairType(t11, t12), PairType(t21, t22)) =>
        (consistent(t11, t21) && consistent(t12, t22))
      case _ => t1 == t2
    }
  }

  def join(t1: Type, t2: Type): Type = {
    (t1, t2) match {
      case (Unknown(), t) => Unknown()
      case (t, Unknown()) => Unknown()
      case (FuncType(t11, t12), FuncType(t21, t22)) =>
        FuncType(join(t11, t21), join(t12, t22))
      case (SumType(t11, t12), SumType(t21, t22)) =>
        SumType(join(t11, t21), join(t12, t22))
      case (PairType(t11, t12), PairType(t21, t22)) =>
        PairType(join(t11, t21), join(t12, t22))
      case _ => if (t1 == t2) t1 else HoleType()
    }
  }
}
