package glang.typing.simple.evidence

import glang.runtime.IRuntimeException
import glang.syntax.Type
import glang.syntax.simple.*
import glang.syntax.simple.TypeOps.*
import glang.typing.{IInteriorError, IMeetError}

object EvidenceOps {
  def meet(t1: EType, t2: EType): Type = {
    (t1, t2) match {
      case (Unknown(), t) => t
      case (t, Unknown()) => t
      case (FuncType(t11, t12), FuncType(t21, t22)) =>
        FuncType(meet(t11, t21), meet(t12, t22))
      case (SumType(t11, t12), SumType(t21, t22)) =>
        SumType(meet(t11, t21), meet(t12, t22))
      case (PairType(t11, t12), PairType(t21, t22)) =>
        PairType(meet(t11, t21), meet(t12, t22))
      case _ => if (t1 == t2) t1 else throw new IMeetError(t1, t2)
    }
  }

  def invdom(ev: Evidence) = Evidence(dom(ev.r), dom(ev.l))

  def invcod(ev: Evidence) = Evidence(cod(ev.l), cod(ev.r))

  def invpi1(ev: Evidence) = Evidence(pi1(ev.l), pi1(ev.r))

  def invpi2(ev: Evidence) = Evidence(pi2(ev.l), pi2(ev.r))

  def invpis1(ev: Evidence) = Evidence(pis1(ev.l), pis1(ev.r))

  def invpis2(ev: Evidence) = Evidence(pis2(ev.l), pis2(ev.r))

  def toPair(ev1: Evidence, ev2: Evidence) =
    Evidence(PairType(ev1.l, ev2.l), PairType(ev1.r, ev2.r))

  def toSuml(ev1: Evidence, ev2: Evidence) =
    Evidence(SumType(ev1.l, ev2.l), SumType(ev1.r, ev2.r))

  def toSumr(ev1: Evidence, ev2: Evidence) =
    Evidence(SumType(ev1.l, ev2.l), SumType(ev1.r, ev2.r))

  def trans(
      ev1: Evidence,
      ev2: Evidence
  ): Either[Evidence, IRuntimeException] = {
    try {
      val m = meet(ev1.r, ev2.l)
      val i1 = interior(ev1.l, m)
      val i2 = interior(m, ev2.r)
      Left(Evidence(i1.l, i2.r))
    } catch {
      case e: IRuntimeException =>
        Right(e)
    }
  }
  implicit def type2Etype(t: Type): EType = t
  def initialEvidence(t1: Type, t2: Type) = interior(t1, t2)
  def interior(t1: EType, t2: EType): Evidence = (t1, t2) match {
    case (Unknown(), t) => Evidence(t, t)
    case (t, Unknown()) => Evidence(t, t)
    case (FuncType(t11, t12), FuncType(t21, t22)) => {
      val ev1 = interior(t21, t11)
      val ev2 = interior(t12, t22)
      Evidence(FuncType(ev1.r, ev2.l), FuncType(ev1.l, ev2.r))
    }
    case (SumType(t11, t12), SumType(t21, t22)) => {
      val ev1 = interior(t11, t21)
      val ev2 = interior(t12, t22)
      Evidence(SumType(ev1.l, ev2.l), SumType(ev1.r, ev2.r))
    }
    case (PairType(t11, t12), PairType(t21, t22)) => {
      val ev1 = interior(t11, t21)
      val ev2 = interior(t12, t22)
      Evidence(PairType(ev1.l, ev2.l), PairType(ev1.r, ev2.r))
    }
    case (t1, t2) if t1 == t2 =>
      Evidence(t1, t2)
    case _ =>
      throw new IInteriorError(t1, t2)
  }
}
