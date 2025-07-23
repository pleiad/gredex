package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.*
import glang.syntax.simple.TypeOps
import glang.syntax.simple.TypeOps.*
import glang.typing.simple.*
import glang.typing.simple.evidence.EvidenceOps.*
import glang.typing.{IHole, IVar}

case class FixHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =
    /*
    IN {fix x. t, ... | ...}
    OUT {t1, [] @ t2, ... | ...}
     */
    case (lstack @ IContext(fix @ IFix(x, t), env) :: lxs, lenv) =>
      val newE = Ops.subst(t, x, fix)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

}
