package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.*
import glang.typing.IHole
import glang.typing.simple.*

case class BooleanHandler(reducer: SimpleReducer)(using
    substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =
    /** IN {v1 op v2, ...} OUT {v1 [op] v2, ...}
      */
    case (lstack @ IContext(IUnOp(t, op), env) :: lxs, lenv) if t.isValue =>
      val newE = Ops.unop(op, t)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /** IN {v1, op [], ...} OUT {op v1, ...} */
    case (c :: IContext(IUnOp(IHole(), op), env) :: lxs, lenv) if c.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IUnOp(c.t, op),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {op t1, ... | ...} OUT {t1, op [], ... | ...}
      */
    case (IContext(IUnOp(t1, op), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IUnOp(IHole(), op),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {v1, if v1 then t2 else t3, ... | ...} OUT {t2|t3, ... | ...}
      */
    case (
          lstack @ IContext(
            t @ IIte(IAsc(IBool(v), ty, e), t2, t3),
            env
          ) :: lxs,
          lenv
        ) =>
      val newE = if (v) IAsc(t2, t.tpe) else IAsc(t3, t.tpe)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /** IN {v1, if [] then t2 else t3, ... | ...} OUT {if v1 then t2 else t3,
      * ... | ...}
      */
    case (c :: IContext(IIte(IHole(), t2, t3), env) :: lxs, lenv)
        if c.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IIte(c.t, t2, t3),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {if t1 then t2 else t3, ... | ...} OUT {t1, if [] then t2 else t3,
      * ... | ...}
      */
    case (IContext(IIte(t1, t2, t3), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IIte(IHole(), t2, t3),
            env
          ) :: lxs,
          lenv
        )
      )

}
