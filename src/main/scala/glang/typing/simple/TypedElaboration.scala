package glang.typing.simple
import glang.syntax.Syntax.Term
import glang.typing.{
  IVar,
  StackError,
  TypedElaboration,
  TypingDerivation,
  VariableNotFoundException
}
import glang.syntax.simple.TypeOps.*
import glang.syntax.{Environments, Hole, Type, Var}
import glang.syntax.simple.*

object TypedElaboration extends TypedElaboration {

  def insertAsc(
      t: TypingDerivation,
      ty: Type,
      stack: List[Frame]
  ): TypingDerivation = {
    stack match {
      case Frame(Asc(Hole(), ty2), env, _) :: xs if ty == ty2 =>
        t // only for optimization
      case _ =>
        t match {
          case IAsc(_, ty2, _, _) if ty == ty2 => t // already an IAsc
          case x if !x.isValue && x.tpe == ty =>
            t // if its a redex it will be an ascription already
          // case x: IVar if x.ty == ty => t
          case _ =>
            IAsc(t, ty) // insert an IAsc
        }
    }
  }

  def apply(
      stack: List[Frame],
      istack: List[TypingDerivation]
  ): TypingDerivation = {

    stack match {
      case Nil => istack.headOption.getOrElse(throw new StackError)

      /** Typ-lit IN: stack: x, ... istack: ... OUT: stack: ... istack: x, ...
        */
      case Frame((t: Bool), env, _) :: xs =>
        apply(xs, insertAsc(IBool(t.b), BoolType(), stack) :: istack)

      /** Typ-lit IN: stack: x, ... istack: ... OUT: stack: ... istack: x, ...
        */
      case Frame((t: Number), env, _) :: xs =>
        apply(xs, insertAsc(INumber(t.v), IntType(), stack) :: istack)

      /** Typ-lit IN: stack: x, ... istack: ... OUT: stack: ... istack: x, ...
        */
      case Frame((t: UnitVal), env, _) :: xs =>
        apply(xs, insertAsc(IUnit(), UnitType(), stack) :: istack)

      /** Typ-var This fails if the variable is not found in the environment IN:
        * stack: x, ... istack: ... OUT: stack: ... istack: x, ...
        */
      case Frame((t: Var), env, _) :: xs => {
        val ty =
          (env.te.lookup(t).getOrElse(throw new VariableNotFoundException(t)))
        apply(xs, IVar(t.x, ty) :: istack)
      }

      /** typ-Abs IN: stack: (\x. []) [], ... istack: e,... OUT: stack: ...
        * istack: (\x. e), ...
        */
      case Frame(Lambda(x, ty, Hole()), env, _) :: xs =>
        istack match {
          case ix :: ixs => {
            apply(
              xs,
              insertAsc(
                ILambda(IVar(x.x, ty), ix),
                FuncType(ty, ix.tpe),
                stack
              ) :: ixs
            )
          }
        }

      /** typ-Abs IN: stack: (\x. e) [], ... istack: ... OUT: stack: e, (\x. [])
        * [], ... istack: ...
        */
      case Frame(Lambda(x, ty, t), env, _) :: xs => {
        val extEnv = env.extend(x -> ty)
        apply(
          c(t, extEnv) :: c(
            Lambda(x, ty, Hole()),
            env
          ) :: xs,
          istack
        )
      }

      /** typ-app IN: stack: [] [], ... istack: e2, e1 ... OUT: stack: ...
        * istack: e1 e2, ...
        */
      case Frame(App(Hole(), Hole()), _, _) :: xs =>
        istack match {
          case it2 :: it1 :: ixs => {
            apply(
              xs,
              IApp(
                insertAsc(it1, FuncType(dom(it1.tpe), cod(it1.tpe)), stack),
                insertAsc(it2, dom(it1.tpe), stack)
              ) :: ixs
            )
          }
          case _ => throw new StackError
        }

      /** typ-app IN: stack: [] e2, ... istack: e1, ... OUT: stack: e2, [] [],
        * ... istack: e1, ...
        */
      case Frame(App(Hole(), t2), env, _) :: xs =>
        istack match {
          case ix :: ixs =>
            apply(
              c(t2, env) :: c(
                App(Hole(), Hole()),
                env
              ) :: xs,
              istack
            )
          case _ => throw new StackError
        }

      /** typ-app IN: stack: e1 e2, ... istack: ... OUT: stack: e1, [] e2, ...
        * istack: ...
        */
      case Frame(App(t1, t2), env, _) :: xs =>
        apply(c(t1, env) :: c(App(Hole(), t2), env) :: xs, istack)

      /** IN: stack: [] op [], ... istack: e2, e1, ... OUT: stack: ... istack:
        * e1 op e2...
        */
      case Frame(BinOp(Hole(), Hole(), op), env, _) :: xs =>
        istack match { // Elimination
          case ix2 :: ix1 :: ixs =>
            val ty = TypeOps.typeOf(op)
            apply(
              xs,
              IBinOp(
                insertAsc(ix1, dom(ty), stack),
                insertAsc(ix2, dom(cod(ty)), stack),
                op
              ) :: ixs
            )
          case _ => throw new StackError
        }

      /** IN: stack: e1 op e2, ... istack: ... OUT: stack: e1, e2, [] op [], ...
        * istack: ...
        */
      case Frame(BinOp(t1, t2, op), env, _) :: xs =>
        apply(
          c(t1, env) :: c(t2, env) :: c(
            BinOp(Hole(), Hole(), op),
            env,
            Infer
          ) :: xs,
          istack
        )

      /** IN: stack: op [], ... istack: e, ... OUT: stack: ... istack: op e...
        */
      case Frame(UnOp(Hole(), op), env, Infer) :: xs =>
        istack match { // Elimination
          case ix1 :: ixs =>
            apply(
              xs,
              IUnOp(insertAsc(ix1, dom(TypeOps.typeOf(op)), stack), op) :: ixs
            )
          case _ => throw new StackError
        }

      /** IN: stack: op e, ... istack: ... OUT: stack: e, op [], ... istack: ...
        */
      case Frame(UnOp(t1, op), env, Infer) :: xs =>
        apply(
          c(t1, env) :: c(UnOp(Hole(), op), env, Infer) :: xs,
          istack
        )

      /** IN: stack: ([], []), ... istack: e2, e1... OUT: stack: ... istack:
        * (e1,e2)...
        */
      case Frame(Pair(Hole(), Hole()), env, Infer) :: xs =>
        istack match { // Elimination
          case ix2 :: ix1 :: ixs =>
            apply(xs, IPair(ix1, ix2) :: ixs)
          case _ => throw new StackError
        }

      /** IN: stack: (e1, e2), ... istack: ... OUT: stack: e1, e2, ([],[]), ...
        * istack: ...
        */
      case Frame(Pair(t1, t2), env, Infer) :: xs =>
        apply(
          c(t1, env, Infer) :: c(t2, env, Infer) :: c(
            Pair(Hole(), Hole()),
            env,
            Infer
          ) :: xs,
          istack
        )

      /** IN: stack: inj1 [], ... istack: e, ... OUT: stack: ... istack: inj1 e,
        * ...
        */
      case Frame(Inl(Hole(), tpe), env, Infer) :: xs =>
        istack match { // Elimination
          case ix :: ixs => apply(xs, IInl(ix, tpe) :: ixs)
          case _         => throw new StackError
        }

      /** IN: stack: inj1 e, ... istack: ... OUT: stack: e, inj1 [], ... istack:
        * ...
        */
      case Frame(Inl(t, tpe), env, Infer) :: xs =>
        apply(
          c(t, env, Infer) :: c(Inl(Hole(), tpe), env, Infer) :: xs,
          istack
        )
      /** same as inj1 */
      case Frame(Inr(Hole(), tpe), env, Infer) :: xs =>
        istack match { // Elimination
          case ix :: ixs => apply(xs, IInr(ix, tpe) :: ixs)
          case _         => throw new StackError
        }
      /** same as inj1 */
      case Frame(Inr(t, tpe), env, Infer) :: xs =>
        apply(
          c(t, env, Infer) :: c(Inr(Hole(), tpe), env, Infer) :: xs,
          istack
        )

      /** IN: stack: if [] then [] else [], ... istack: e3, e2, e1... OUT:
        * stack: ... istack: if e1 then e2 else e3...
        */
      case Frame(Ite(Hole(), Hole(), Hole()), env, Infer) :: xs =>
        istack match {
          case it3 :: it2 :: it1 :: ixs => {
            val eqty = join(it2.tpe, it3.tpe)
            apply(
              xs,
              IIte(
                insertAsc(it1, BoolType(), stack),
                insertAsc(it2, eqty, stack),
                insertAsc(it3, eqty, stack)
              ) :: ixs
            )
          }
          case _ => throw new StackError
        }
      /** IN: stack: if e1 then e2 else e3, ... istack: ... OUT: stack: e1, e2,
        * e3 if [] then [] else [], ... istack: ...
        */
      case Frame(Ite(t1, t2, t3), env, Infer) :: xs =>
        apply(
          c(t1, env) :: c(t2, env, Infer) :: c(t3, env, Infer) :: c(
            Ite(Hole(), Hole(), Hole()),
            env,
            Infer
          ) :: xs,
          istack
        )

      /** typ-anno IN: stack: [] :: T, ... istack: e, ... OUT: stack: ...
        * istack: e :: T, ...
        */
      case Frame(Asc(Hole(), t), env, Infer) :: xs =>
        istack match {
          case ie :: ixs => {
            apply(xs, IAsc.source(ie, t) :: ixs)
          }
          case _ => throw new StackError
        }

      /** typ-anno IN: stack: e :: T, ... istack: ... OUT: stack: e, [] :: T,
        * ...
        */
      case Frame(Asc(e, ot), env, Infer) :: xs => {
        apply(c(e, env) :: c(Asc(Hole(), ot), env, Infer) :: xs, istack)
      }

      /** IN: stack: let _ = [] in [], ... istack: e2, x, e1, ... OUT: stack:
        * ... istack: let x = e1 in e2, ...
        */
      case Frame(Let(_, Hole(), Hole()), env, Infer) :: xs =>
        istack match {
          case it2 :: (ix: IVar) :: it1 :: ixs => {
            apply(xs, ILet(ix, it1, it2) :: ixs)
          }
          case _ => throw new StackError
        }

      /** IN: stack: let x = [] in e2, ... istack: e1, ... OUT: stack: e2, let x
        * \= [] in [], ... istack: e1, ...
        */
      case Frame(Let(x, Hole(), t2), env, Infer) :: xs =>
        istack match {
          case it1 :: ixs => {
            val newEnv = env.extend(x -> it1.tpe)
            apply(
              c(x, newEnv, Infer) :: c(t2, newEnv, Infer) :: c(
                Let(x, Hole(), Hole()),
                env,
                Infer
              ) :: xs,
              istack
            )
          }
          case _ => throw new StackError
        }

      /** IN: stack: let x = e1 in e2, ... istack: ... OUT: stack: e1, let x =
        * [] in e2, ... istack: ...
        */
      case Frame(Let(x, t1, t2), env, Infer) :: xs =>
        apply(
          c(t1, env, Infer) :: c(Let(x, Hole(), t2), env, Infer) :: xs,
          istack
        )

      /** IN: stack first [], ... istack: e, ... OUT: stack: ... istack: fst e,
        * ...
        */
      case Frame(First(Hole()), env, Infer) :: xs =>
        istack match {
          case it :: ixs =>
            val tpe = it.tpe
            val tpe1 = pi1(tpe)
            val tpe2 = pi2(tpe)
            apply(xs, IFirst(insertAsc(it, PairType(tpe1, tpe2), stack)) :: ixs)
          case _ => throw new StackError
        }

      /** IN: stack: fst e, ... istack: ... OUT: stack: e, fst [], ... istack:
        * ...
        */
      case Frame(First(t), env, Infer) :: xs =>
        apply(c(t, env, Infer) :: c(First(Hole()), env, Infer) :: xs, istack)

      /** analogous to First */
      case Frame(Second(Hole()), env, Infer) :: xs =>
        istack match {
          case it :: ixs =>
            val tpe = it.tpe
            apply(xs, ISecond(it) :: ixs)
          case _ => throw new StackError
        }
      /** analogous to First */
      case Frame(Second(t), env, Infer) :: xs =>
        apply(c(t, env, Infer) :: c(Second(Hole()), env, Infer) :: xs, istack)

      /** IN: stack: case [] of x1 => [] | x2 => [], ... istack: e2, x2, e1, x1,
        * e, ... OUT: stack: ... istack: case e of x1 => e1 | x2 => e2, ...
        */
      case Frame(Case(Hole(), _, Hole(), _, Hole()), env, Infer) :: xs =>
        istack match {
          case it2 :: (ix2: IVar) :: it1 :: (ix1: IVar) :: it :: ixs => {
            val ty = join(it1.tpe, it2.tpe)
            apply(
              xs,
              ICase(
                it,
                ix1,
                insertAsc(it1, ty, stack),
                ix2,
                insertAsc(it2, ty, stack)
              ) :: ixs
            )
          }
          case _ => throw new StackError
        }

      /** IN: stack: case [] of x1 => e1 | x2 => e2, ... istack: e, ... OUT:
        * stack: x1, e1, x2, e2, case [] of [] => [] | [] => [], ... istack: e,
        * ...
        */
      case Frame(Case(Hole(), x1, t1, x2, t2), env, Infer) :: xs =>
        istack match {
          case it :: ixs => {
            val t = it.tpe
            val ty1 = pis1(t)
            val ty2 = pis2(t)
            val newEnv1 = env.extend(x1 -> ty1)
            val newEnv2 = env.extend(x2 -> ty2)
            apply(
              c(x1, newEnv1, Infer) :: c(t1, newEnv1, Infer) :: c(
                x2,
                newEnv2,
                Infer
              ) :: c(t2, newEnv2, Infer) :: c(
                Case(Hole(), x1, Hole(), x2, Hole()),
                env,
                Infer
              ) :: xs,
              istack
            )
          }
          case _ => throw new StackError
        }

      /** IN: stack: case e of x1 => e1 | x2 => e2, ... istack: ... OUT: stack:
        * e, case [] of x1 => e1 | x2 => e2, ... istack: ...
        */
      case Frame(Case(t, x1, t1, x2, t2), env, Infer) :: xs =>
        apply(
          c(t, env, Infer) :: c(Case(Hole(), x1, t1, x2, t2), env, Infer) :: xs,
          istack
        )

      /* typ-fix
        IN: stack: (fix x. []) [], ... istack: e, ...
        OUT: stack: ... istack: (fix x. e), ...
       */
      case Frame(Fix(x, ty, Hole()), env, _) :: xs =>
        istack match {
          case ix :: ixs => {
            apply(xs, IFix(IVar(x.x, ty), insertAsc(ix, ty, stack)) :: ixs)
          }
        }

      /* typ-fix
        IN: stack: (fix x. e) [], ... istack: ...
        OUT: stack: e, (fix x. []) [], ... istack: ...
       */
      case Frame(Fix(x, ty, t), env, _) :: xs => {
        val extEnv = env.extend(x -> ty)
        apply(
          c(t, extEnv) :: c(
            Fix(x, ty, Hole()),
            env
          ) :: xs,
          istack
        )
      }

      /** default case is an error
        */
      case c => throw new Error("Typing not implemented for " + c)
    }
  }

  def stack2String(s: List[Frame]): String = {
    s match {
      case Nil => ""
      case x :: xs => {
        x.pprint + "\n" + stack2String(xs)
      }
    }
  }
}
