package glang.runtime.simple.handlers

import glang.runtime.LinearEnv
import glang.runtime.SubstitutionModel.SubstitutionModel
import glang.runtime.simple.{
  Frames,
  IContext,
  More,
  Ops,
  SimpleReducer,
  Step,
  Trampoline
}
import glang.typing.IHole
import glang.typing.simple.*

case class OpsHandler(reducer: SimpleReducer)(using
                                              substitutionModel: SubstitutionModel
) {
  def cases: PartialFunction[(Frames, LinearEnv), Trampoline] =
    /** IN {v1 op v2, ...} OUT {v1 [op] v2, ...}
      */
    case (lstack @ IContext(IBinOp(t1, t2, op), env) :: lxs, lenv)
        if t1.isValue && t2.isValue =>
      val newE = Ops.binop(op, t1, t2)
      Step(
        reducer,
        lstack,
        lenv,
        IContext(newE, env) :: lxs,
        lenv,
        (l) => reducer.reduce(l, lenv),
        List(newE)
      )

    /** IN {v1 , [] op t2, ...} OUT {v1 op t2, ...} */
    case (c1 :: IContext(IBinOp(IHole(), t2, op), env) :: lxs, lenv)
        if c1.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IBinOp(c1.t, t2, op),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {v1 op t2, ...} OUT {t2, v1 op [], ...} */
    case (IContext(IBinOp(t1, t2, op), env) :: lxs, lenv)
        if t1.isValue && !t2.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t2, env) :: IContext(
            IBinOp(t1, IHole(), op),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {v2, v1 op [], ...} OUT {v1 op v2, ...} */
    case (c2 :: IContext(IBinOp(v1, IHole(), op), env) :: lxs, lenv)
        if c2.t.isValue =>
      More(() =>
        reducer.reduce(
          IContext(
            IBinOp(v1, c2.t, op),
            env
          ) :: lxs,
          lenv
        )
      )

    /** IN {t1 op t2, ... | ...} OUT {t1, [] op t2, ... | ...}
      */
    case (IContext(IBinOp(t1, t2, op), env) :: lxs, lenv) if !t1.isValue =>
      More(() =>
        reducer.reduce(
          IContext(t1, env) :: IContext(
            IBinOp(IHole(), t2, op),
            env
          ) :: lxs,
          lenv
        )
      )

}
