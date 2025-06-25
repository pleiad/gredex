package lang.runtime.simple.handlers

import lang.runtime.LinearEnv
import lang.runtime.SubstitutionModel.SubstitutionModel
import lang.runtime.simple.*
import lang.syntax.simple.{PairType, TypeOps}
import lang.syntax.simple.TypeOps.*
import lang.typing.simple.*
import lang.typing.simple.evidence.EvidenceOps.*
import lang.typing.{IHole, IVar}

case class PairHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =

    /*
    IN { (v1 :: T1,v2 :: T2) :: T, ... }
    OUT { (v1,v2) :: T1xT2 :: T ... }
     */
    case (
          lstack @ IContext(
            v @ IPair(v1 @ IAsc(v1t, t1, ev1), v2 @ IAsc(v2t, t2, ev2)),
            env
          ) :: lxs,
          lenv
        ) if v1.isValue && v2.isValue =>
      val newE =
        IAsc(IPair(v1t, v2t), PairType(t1, t2), toPair(ev1, ev2))
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
   IN { fst (v1,v2), ... }
   OUT { v1 ... }
     */
    case (
          lstack @ IContext(
            IFirst(v @ IAsc(IPair(v1, v2), PairType(t1, t2), ev)),
            env
          ) :: lxs,
          lenv
        ) if v.isValue =>
      val newE = IAsc(v1, t1, invpi1(ev))
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
    IN { v1, fst [], ... }
    OUT { fst v1 ... }
     */
    case (c1 :: IContext(IFirst(IHole()), env) :: lxs, lenv) if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(IFirst(c1.t), env) :: lxs,
          lenv
        )
      )

    /*
    IN { fst t1, ... }
    OUT { t1, fst [] ... }
     */
    case (IContext(IFirst(t1), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(IFirst(IHole()), env) :: lxs,
          lenv
        )
      )

    /*
    IN { snd (v1,v2), ... }
    OUT { v1 ... }
     */
    case (
          lstack @ IContext(
            ISecond(v @ IAsc(IPair(v1, v2), PairType(t1, t2), ev)),
            env
          ) :: lxs,
          lenv
        ) if v.isValue =>
      val newE = IAsc(v2, t2, invpi2(ev))
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
    IN { v1, fst [], ... }
    OUT { snd v1 ... }
     */
    case (c1 :: IContext(ISecond(IHole()), env) :: lxs, lenv) if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(ISecond(c1.t), env) :: lxs,
          lenv
        )
      )

    /*
    IN { fst t1, ... }
    OUT { t1, fst [] ... }
     */
    case (IContext(ISecond(t1), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(ISecond(IHole()), env) :: lxs,
          lenv
        )
      )

    /*
    IN {v1 , ([], t2), ...}
    OUT {(v1, t2), ...}
     */
    case (c1 :: IContext(IPair(IHole(), t2), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IPair(c1.t, t2),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {(v1, t2), ...}
    OUT {t2, (v1, []), ...}
     */
    case (IContext(IPair(t1, t2), env) :: lxs, lenv)
        if t1.isValue && !t2.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t2, env) :: IContext(
            IPair(t1, IHole()),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {v2, (v1, []), ...}
    OUT {(v1, v2), ...}
     */
    case (c2 :: IContext(IPair(v1, IHole()), env) :: lxs, lenv)
        if c2.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IPair(v1, c2.t),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {(t1, t2), ... | ...}
    OUT {t1, ([], t2), ... | ...}
     */
    case (IContext(IPair(t1, t2), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IPair(IHole(), t2),
            env
          ) :: lxs,
          lenv
        )
      )

}
