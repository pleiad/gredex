package lang.runtime.simple.handlers

import lang.runtime.LinearEnv
import lang.runtime.SubstitutionModel.SubstitutionModel
import lang.runtime.simple.*
import lang.syntax.simple.TypeOps
import lang.syntax.simple.TypeOps.*
import lang.typing.{IHole, TypingDerivation}
import lang.typing.simple.*
import lang.typing.simple.evidence.EvidenceOps.*

case class AscriptionHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {

  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =
    /** IN {v1 @ v2, ...} OUT {v1 [op] v2, ...}
      */
    case (
          lstack @ IContext(
            IAsc(t1 @ IAsc(v, ty1, ev1), ty2, ev2),
            env
          ) :: lxs,
          lenv
        ) if t1.isValue =>
      trans(ev1, ev2) match {
        case Left(ev) =>
          val newE = IAsc(v, ty2, ev)
          Step(
            reducer,
            lstack,
            lenv,
            IContext(newE, env) :: lxs,
            lenv,
            (l) => reducer.reduce(l, lenv),
            List(newE)
          )
        case Right(e) => IError(reducer, lstack, lenv, e, List(ev1, ev2))
      }

    /** IN {v1 , [] :: T, ...} OUT {v1 :: T, ...} */
    case (c1 :: IContext(IAsc(IHole(), ty, ev), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IAsc(c1.t, ty, ev),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {t1 :: T, ... | ...} OUT {t1, [] :: T, ... | ...}
      */
    case (IContext(IAsc(t, ty, ev), env) :: lxs, lenv) if !t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t, env) :: IContext(
            IAsc(IHole(), ty, ev),
            env
          ) :: lxs,
          lenv
        )
      )

}
