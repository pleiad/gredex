package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.*
import glang.syntax.simple.TypeOps.*
import glang.syntax.simple.{PairType, SumType, TypeOps}
import glang.typing.simple.*
import glang.typing.simple.evidence.EvidenceOps.*
import glang.typing.{IHole, IVar}

case class SumHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =

    /*
    IN { (inl v1 :: T1) :: T, ... }
    OUT { (inl v1) :: T1+pis2(T) :: T ... }
     */
    case (
          lstack @ IContext(
            v @ IInl(v1 @ IAsc(v1t, t1, ev1, s), ty),
            env
          ) :: lxs,
          lenv
        ) if !v.isValue =>
      val eright = interior(ty, ty)
      val newE =
        IAsc(IInl(v1t, ty), SumType(t1, ty), toSuml(ev1, eright), s)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /*
    IN { (inr v1 :: T1) :: T, ... }
    OUT { (inr v1) :: pis1(T) + T1 :: T ... }
     */
    case (
          lstack @ IContext(
            v @ IInr(v1 @ IAsc(v1t, t1, ev1, s), ty),
            env
          ) :: lxs,
          lenv
        ) if !v.isValue =>
      val eleft = interior(ty, ty)
      val newE =
        IAsc(IInr(v1t, ty), SumType(ty, t1), toSumr(eleft, ev1), s)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /*
    IN { case inl v1 x1 t1 x2 t2, ... }
    OUT { t1[v1/x1] ... }
     */
    case (
          lstack @ IContext(
            ICase(v @ IAsc(IInl(v1, _), ty, ev, s), x1, t1, x2, t2),
            env
          ) :: lxs,
          lenv
        ) if v.isValue =>
      val newE = Ops.subst(t1, x1, IAsc(v1, pis1(ty), invpis1(ev), s))
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )
    /*
  IN { case inr v1 x1 t1 x2 t2, ... }
  OUT { t2[v1/x2] ... }
     */
    case (
          lstack @ IContext(
            ICase(v @ IAsc(IInr(v1, _), ty, ev, s), x1, t1, x2, t2),
            env
          ) :: lxs,
          lenv
        ) if v.isValue =>
      val newE = Ops.subst(t2, x2, IAsc(v1, pis2(ty), invpis2(ev), s))
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /*
    IN { v1, case [] x1 t1 x2 t2, ... }
    OUT { case v1 x1 t1 x2 t2 ... }
     */
    case (c1 :: IContext(ICase(IHole(), x1, t1, x2, t2), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(ICase(c1.t, x1, t1, x2, t2), env) :: lxs,
          lenv
        )
      )

    /*
    IN { case t1 x1 t1 x2 t2 , ... }
    OUT { t1, case [] x1 t1 x2 t2 ... }
     */
    case (IContext(ICase(t, x1, t1, x2, t2), env) :: lxs, lenv) if !t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t, env) :: IContext(
            ICase(IHole(), x1, t1, x2, t2),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
  IN {v, inr [], ...}
  OUT {inr v, ...}
     */
    case (c1 :: IContext(IInr(IHole(), ty), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IInr(c1.t, ty),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {inr t1, ... | ...}
    OUT {t1, inr [], ... | ...}
     */
    case (IContext(IInr(t1, ty), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IInr(IHole(), ty),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {v, inl [], ...}
    OUT {inl v, ...}
     */
    case (c1 :: IContext(IInl(IHole(), ty), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IInl(c1.t, ty),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {inl t1, ... | ...}
    OUT {t1, inl [], ... | ...}
     */
    case (IContext(IInl(t1, ty), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IInl(IHole(), ty),
            env
          ) :: lxs,
          lenv
        )
      )

}
