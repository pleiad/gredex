package lang.runtime.simple.handlers

import lang.runtime.LinearEnv
import lang.runtime.SubstitutionModel.SubstitutionModel
import lang.runtime.simple.*
import lang.syntax.simple.TypeOps
import lang.syntax.simple.TypeOps.*
import lang.typing.simple.*
import lang.typing.simple.evidence.EvidenceOps.*
import lang.typing.{IHole, IVar}

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
