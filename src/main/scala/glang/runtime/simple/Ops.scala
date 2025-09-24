package glang.runtime.simple

import glang.syntax.Syntax
import glang.syntax.simple.*
import glang.typing.{IHole, IVar, TypingDerivation}
import glang.typing.simple.*

/** Provides operational semantics for evaluating and transforming typed
  * expressions. Includes:
  *   - Evaluation of unary and binary operations (`unop`, `binop`)
  *   - Type-annotated reconstruction of intermediate derivations
  *     (`reconstructITerm`)
  *   - Substitution of variables in type derivations (`subst`)
  */
object Ops {

  /** Applies a unary operator to a typed value.
    *
    * @param op
    *   Unary operator (e.g., "!")
    * @param v
    *   Typed operand
    * @return
    *   The result of the operation, with type information
    */
  def unop(op: String, v: TypingDerivation): TypingDerivation = (op, v) match {
    case (Syntax.notS.parser | "not", IAsc(IBool(b), _, _, _)) =>
      IAsc(IBool(!b), BoolType())
  }

  /** Applies a binary operator to two typed values.
    *
    * @param op
    *   Binary operator (e.g., "+", "==")
    * @param v1
    *   First operand
    * @param v2
    *   Second operand
    * @return
    *   The result of the operation, with type information
    * @throws RuntimeException
    *   if division by zero occurs
    */
  def binop(
      op: String,
      v1: TypingDerivation,
      v2: TypingDerivation
  ): TypingDerivation =
    (op, v1, v2) match {
      // Arithmetic operations
      case ("+", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(INumber(n1 + n2), IntType())
      case ("-", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(INumber(n1 - n2), IntType())
      case ("*", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(INumber(n1 * n2), IntType())
      case ("/", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        if (n2 == 0) throw new RuntimeException("Division by zero")
        else IAsc(INumber(n1 / n2), IntType())

      // Comparisons
      case ("==", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(IBool(n1 == n2), BoolType())
      case ("<", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(IBool(n1 < n2), BoolType())
      case (">", IAsc(INumber(n1), _, _, _), IAsc(INumber(n2), _, _, _)) =>
        IAsc(IBool(n1 > n2), BoolType())

      // Logical operators
      case (
            Syntax.andS.parser,
            IAsc(IBool(b1), _, _, _),
            IAsc(IBool(b2), _, _, _)
          ) =>
        IAsc(IBool(b1 && b2), BoolType())
      case (
            Syntax.orS.parser,
            IAsc(IBool(b1), _, _, _),
            IAsc(IBool(b2), _, _, _)
          ) =>
        IAsc(IBool(b1 || b2), BoolType())
    }

  /** Reconstructs a fully typed term from a list of evaluation frames.
    *
    * @param lstack
    *   The linear stack of evaluation contexts
    * @return
    *   The fully reconstructed term with types
    */
  def reconstructITerm(lstack: Frames): TypingDerivation = lstack match {
    case IContext(t, env) :: Nil => t

    // Binary operators
    case c1 :: IContext(IBinOp(IHole(), t2, op), env) :: xs =>
      reconstructITerm(IContext(IBinOp(c1.t, t2, op), env) :: xs)
    case c2 :: IContext(IBinOp(t1, IHole(), op), env) :: xs =>
      reconstructITerm(IContext(IBinOp(t1, c2.t, op), env) :: xs)

    // Unary operator
    case c1 :: IContext(IUnOp(IHole(), op), env) :: xs =>
      reconstructITerm(IContext(IUnOp(c1.t, op), env) :: xs)

    // If-then-else
    case c1 :: IContext(IIte(IHole(), t2, t3), env) :: xs =>
      reconstructITerm(IContext(IIte(c1.t, t2, t3), env) :: xs)

    // Function application
    case c1 :: IContext(IApp(IHole(), t2), env) :: xs =>
      reconstructITerm(IContext(IApp(c1.t, t2), env) :: xs)
    case c2 :: IContext(IApp(t1, IHole()), env) :: xs =>
      reconstructITerm(IContext(IApp(t1, c2.t), env) :: xs)

    // Ascription
    case c1 :: IContext(IAsc(IHole(), ty, ev, s), env) :: xs =>
      reconstructITerm(IContext(IAsc(c1.t, ty, ev, s), env) :: xs)

    // Fixpoint
    case c1 :: IContext(IFix(x, IHole()), env) :: xs =>
      reconstructITerm(IContext(IFix(x, c1.t), env) :: xs)

    // Let
    case c1 :: IContext(ILet(x, IHole(), t2), env) :: xs =>
      reconstructITerm(IContext(ILet(x, c1.t, t2), env) :: xs)

    // Pairs
    case c1 :: IContext(IPair(IHole(), t2), env) :: xs =>
      reconstructITerm(IContext(IPair(c1.t, t2), env) :: xs)
    case c2 :: IContext(IPair(t1, IHole()), env) :: xs =>
      reconstructITerm(IContext(IPair(t1, c2.t), env) :: xs)
    case c1 :: IContext(IFirst(IHole()), env) :: xs =>
      reconstructITerm(IContext(IFirst(c1.t), env) :: xs)
    case c2 :: IContext(ISecond(IHole()), env) :: xs =>
      reconstructITerm(IContext(ISecond(c2.t), env) :: xs)

    // Sum types
    case c1 :: IContext(IInl(IHole(), ty), env) :: xs =>
      reconstructITerm(IContext(IInl(c1.t, ty), env) :: xs)
    case c2 :: IContext(IInr(IHole(), ty), env) :: xs =>
      reconstructITerm(IContext(IInr(c2.t, ty), env) :: xs)
    case c1 :: IContext(ICase(IHole(), x1, t1, x2, t2), env) :: xs =>
      reconstructITerm(IContext(ICase(c1.t, x1, t1, x2, t2), env) :: xs)
  }

  /** Substitutes variable `x` with value `v` inside expression `e`. Variable
    * binding is respected to avoid capture.
    *
    * @param e
    *   The expression to transform
    * @param x
    *   The variable to substitute
    * @param v
    *   The value to substitute in place of x
    * @return
    *   A new expression where x has been replaced with v
    */
  def subst(
      e: TypingDerivation,
      x: IVar,
      v: TypingDerivation
  ): TypingDerivation = e match {
    case IHole()            => IHole()
    case INumber(n)         => INumber(n)
    case IBinOp(t1, t2, op) => IBinOp(subst(t1, x, v), subst(t2, x, v), op)
    case IBool(b)           => IBool(b)
    case IIte(t1, t2, t3) =>
      IIte(subst(t1, x, v), subst(t2, x, v), subst(t3, x, v))
    case IUnOp(t, op) =>
      IUnOp(subst(t, x, v), op)

    // Variables
    case IVar(y, ty) if y == x.x => v
    case IVar(y, ty)             => IVar(y, ty)

    // Lambda abstraction
    case ILambda(y, e) if x.x != y.x =>
      ILambda(y, subst(e, x, v))
    case ILambda(y, e) if x.x == y.x =>
      ILambda(y, e)

    // Application
    case IApp(t1, t2) =>
      IApp(subst(t1, x, v), subst(t2, x, v))

    // Ascription
    case IAsc(t, ty, ev, s) =>
      IAsc(subst(t, x, v), ty, ev, s)

    // Fix
    case IFix(y, e) =>
      if (x.x == y.x) e else IFix(y, subst(e, x, v))

    // Let
    case ILet(y, t1, t2) =>
      if (x.x == y.x) ILet(y, t1, t2)
      else ILet(y, subst(t1, x, v), subst(t2, x, v))

    // Pairs
    case IPair(t1, t2) =>
      IPair(subst(t1, x, v), subst(t2, x, v))
    case IFirst(t) =>
      IFirst(subst(t, x, v))
    case ISecond(t) =>
      ISecond(subst(t, x, v))

    // Sums
    case IInl(t, ty) =>
      IInl(subst(t, x, v), ty)
    case IInr(t, ty) =>
      IInr(subst(t, x, v), ty)
    case ICase(t, x1, t1, x2, t2) =>
      ICase(
        subst(t, x, v),
        x1,
        if (x1.x == x.x) t1 else subst(t1, x, v),
        x2,
        if (x2.x == x.x) t2 else subst(t2, x, v)
      )
  }

}
