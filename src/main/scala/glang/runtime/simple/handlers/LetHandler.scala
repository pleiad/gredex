package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.*
import glang.syntax.simple.TypeOps
import glang.syntax.simple.TypeOps.*
import glang.typing.simple.*
import glang.typing.simple.evidence.EvidenceOps.*
import glang.typing.{IHole, IVar}

case class LetHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =

    /*
    IN {let x = v1 in t2, ...}
    OUT {t2[v1/x], ...}
     */
    case (
          lstack @ IContext(
            ILet(x, t1, t2),
            env
          ) :: lxs,
          lenv
        ) if t1.isValue =>
      val newE = Ops.subst(t2, x, t1)
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
    IN {v1, let x = [] in t2, ... | ...}
    OUT {let x = v1 in t2, ... | ...}
     */
    case (c1 :: IContext(ILet(x, IHole(), t2), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            ILet(x, c1.t, t2),
            env
          ) :: lxs,
          lenv
        )
      )

    /*
    IN {let x = t1 in t2, ... | ...}
    OUT {t1, let x = [] in t2, ... | ...}
     */
    case (IContext(ILet(x, t1, t2), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            ILet(x, IHole(), t2),
            env
          ) :: lxs,
          lenv
        )
      )

}
