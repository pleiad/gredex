package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.*
import glang.syntax.simple.TypeOps
import glang.typing.{IHole, IVar}
import glang.typing.simple.*
import TypeOps.*
import glang.typing.simple.evidence.EvidenceOps.*

case class FunctionHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =

    /** IN {v1 @ v2, ...} OUT {v1 [op] v2, ...} */
    case (
          lstack @ IContext(
            IApp(
              t1 @ IAsc(ILambda(x, e), ty1, ev1, s1),
              t2 @ IAsc(v, ty2, ev2, s2)
            ),
            env
          ) :: lxs,
          lenv
        ) if t1.isValue && t2.isValue =>
      trans(ev2, invdom(ev1)) match {
        case Left(ev) =>
          val newV = IAsc(v, x.tpe, ev, s1)
          val newE = IAsc(Ops.subst(e, x, newV), cod(ty1), invcod(ev1), s1)
          Step(
            reducer,
            lstack,
            lenv,
            IContext(newE, env) :: lxs,
            lenv,
            (l) => reducer.reduce(l, lenv),
            List(newE)
          )
        case Right(e) => IError(reducer, lstack, lenv, e, List(ev1))
      }

    /** IN {v1 , [] @ t2, ...} OUT {v1 @ t2, ...} */
    case (c1 :: IContext(IApp(IHole(), t2), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IApp(c1.t, t2),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {v1 @ t2, ...} OUT {t2, v1 @ [], ...} */
    case (IContext(IApp(t1, t2), env) :: lxs, lenv)
        if t1.isValue && !t2.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t2, env) :: IContext(
            IApp(t1, IHole()),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {v2, v1 @ [], ...} OUT {v1 @ v2, ...} */
    case (c2 :: IContext(IApp(v1, IHole()), env) :: lxs, lenv)
        if c2.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IApp(v1, c2.t),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {t1 @ t2, ... | ...} OUT {t1, [] @ t2, ... | ...}
      */
    case (IContext(IApp(t1, t2), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IApp(IHole(), t2),
            env
          ) :: lxs,
          lenv
        )
      )

}
