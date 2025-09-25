package glang.typing.simple

import glang.syntax.Type
import glang.typing.{IOptions, Judgement}
import glang.typing.simple.evidence.Evidence

/** This class represents a consistency judgment.
  *
  * @param t1
  *   The first/lhs type
  * @param t2
  *   The second/rhs type
  */
case class Consistency(t1: Type, t2: Type, ev: Evidence) extends Judgement {
  val op = "\\sim"

  def toLatex(implicit o: IOptions): String = {
    val judgment =
      s"${if (!o.hideEvidences) s"${ev.toLatex} \\vdash" else ""}  ${t1.toLatex} $op ${t2.toLatex}"
    judgment
  }

  def isReflexive(): Boolean = t1 == t2
}
