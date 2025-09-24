package glang.typing.simple

import glang.runtime.RuntimeEnvironment
import glang.syntax.simple.*
import glang.typing.{
  ICheckedValue,
  IOptions,
  ISerious,
  ITypeError,
  ITypeNotFound,
  IVar,
  SimpleValue,
  TypingDerivation
}
import glang.syntax.Syntax.*
import glang.syntax.Type
import glang.syntax.simple.TypeOps.*
import glang.typing.simple.evidence.{Evidence, EvidenceOps}

/** A derivation tree of a number
  *
  * @param v
  *   The integer value
  */
case class INumber(v: Int) extends SimpleValue {
  def tpe = IntType()

  def pprint = v.toString

  def toLatex(implicit o: IOptions): String = postProcess(pprint)

  def derivationName = "TLit"
}

/** A derivation tree of a boolean
  * @param b
  *   The boolean value
  */
case class IBool(b: Boolean) extends SimpleValue {
  def tpe = BoolType()

  def pprint = b.toString

  def toLatex(implicit o: IOptions): String = postProcess(
    s"\\mathtt{${pprint}}"
  )

  def derivationName = "TLit"
}

/** A derivation tree of a unit value
  */
case class IUnit() extends SimpleValue {

  def tpe = UnitType()

  def pprint = "()"

  def toLatex(implicit o: IOptions): String = postProcess("()")

  def derivationName = "TLit"
}

/** A derivation tree of a binary operation
  * @param t1
  *   The first term in the operation
  * @param t2
  *   The second term in the operation
  * @param op
  *   The operation to perform
  */
case class IBinOp(t1: TypingDerivation, t2: TypingDerivation, op: String)
    extends ISerious {
  val (dom1, dom2, cod) = TypeOps.typeOf(op) match {
    case FuncType(t1, FuncType(t2, t3)) =>
      (t1, t2, t3)
    case _ =>
      throw new ITypeNotFound(op)
  }

  def tpe = {
    if ((t1.tpe == dom1) && (t2.tpe == dom2)) cod
    else if (t1.tpe != dom1)
      throw new ITypeError(t1, t1.tpe, dom1)
    else throw new ITypeError(t2, t2.tpe, dom2)
  }

  def pprint = t1.pprint + " " + op + " " + t2.pprint

  override def debug = t1.debug + " " + op + " " + t2.debug

  def opToLatex(op: String) = op match {
    case andS.parser => "\\wedge"
    case orS.parser  => "\\vee"
    case "<"         => "\\lt"
    case ">"         => "\\gt"
    case _           => op
  }

  def toLatex(implicit o: IOptions) = postProcess(
    "(" + t1.toLatex + " " + opToLatex(op) + " " + t2.toLatex + ")"
  )

  override val subTerms = Seq(t1, t2)

  def derivationName = "TBop"
}

/** A derivation tree of a boolean unary operation
  * @param t1
  *   The term to apply the operation to
  * @param op
  *   The operation to perform
  */
case class IUnOp(t1: TypingDerivation, op: String) extends ISerious {
  val (dom, cod) = TypeOps.typeOf(op) match {
    case FuncType(t1, t2) =>
      (t1, t2)
    case _ =>
      throw new ITypeNotFound(op)
  }
  def tpe = if (t1.tpe == dom) cod
  else {
    throw new ITypeError(t1, t1.tpe, dom)
  }

  def pprint = op + " " + t1.pprint

  override def debug = op + " " + t1.debug

  def opToLatex(op: String) = op match {
    case _ => op
  }

  def toLatex(implicit o: IOptions) = postProcess(
    "(" + opToLatex(op) + " " + t1.toLatex + ")"
  )

  override val subTerms = Seq(t1)

  def derivationName = "TUop"
}

/** A derivation tree of a pair of terms
  * @param t1
  *   The first term in the pair
  * @param t2
  *   The second term in the pair
  */
case class IPair(t1: TypingDerivation, t2: TypingDerivation) extends ISerious {

  override def isValue: Boolean = {
    (t1, t2) match {
      case (t1: IAsc, _) => false
      case (_, t2: IAsc) => false
      case _             => t1.isValue && t2.isValue
    }

  }
  def tpe = PairType(t1.tpe, t2.tpe)

  def pprint = "<" + t1.pprint + ", " + t2.pprint + ">"

  override def debug = "P<" + t1.debug + ", " + t2.debug + ">"

  def toLatex(implicit o: IOptions) = postProcess(
    "(" + t1.toLatex + ", " + t2.toLatex + ")"
  )

  override val subTerms = Seq(t1, t2)

  def derivationName = "TPair"
}

/** A derivation tree of a left injection
  * @param t
  *   The term to inject
  * @param tpe2
  *   The type of the sum type
  */
case class IInl(t: TypingDerivation, tpe2: Type) extends ISerious {
  override def isValue = t match {
    case IAsc(_, _, _) => false
    case _             => t.isValue
  }

  def tpe = SumType(t.tpe, tpe2)

  def pprint = "Pinj1(" + t.pprint + ")"

  override def debug = "Pinj1(" + t.debug + ")"

  def toLatex(implicit o: IOptions) = postProcess(
    s"\\mathsf{inl}^{${tpe2.toLatex}}~${t.toLatex}"
  )

  override val subTerms = Seq(t)

  def derivationName = "TInl"
}

/** A derivation tree of a right injection
  * @param t
  *   The term to inject
  * @param tpe2
  *   The type of the sum type
  */
case class IInr(t: TypingDerivation, tpe2: Type) extends ISerious {
  override def isValue = t match {
    case IAsc(_, _, _) => false
    case _             => t.isValue
  }

  def tpe = SumType(tpe2, t.tpe)

  def pprint = "Pinj2(" + t.pprint + ")"

  override def debug = "Vinj2(" + t.debug + ")"

  def toLatex(implicit o: IOptions) = postProcess(
    s"\\mathsf{inr}^{${tpe2.toLatex}}~${t.toLatex}"
  )

  override val subTerms = Seq(t)

  def derivationName = "TInr"
}

/** A derivation tree of an if-then-else
  * @param t1
  *   The condition
  * @param t2
  *   The then branch
  * @param t3
  *   The else branch
  */
case class IIte(
    t1: TypingDerivation,
    t2: TypingDerivation,
    t3: TypingDerivation
) extends ISerious {

  def tpe = {

    if ((t1.tpe == BoolType()) && (t2.tpe == t3.tpe)) t2.tpe
    else {
      if (!(t1.tpe == BoolType()))
        throw new ITypeError(this, t1.tpe, BoolType())
      else {
        print("t1: " + t1.tpe + ", t2: " + t2.tpe + ", t3: " + t3.tpe)
        throw new ITypeError(t3, t3.tpe, t2.tpe)
      }
    }
  }

  def pprint = "if " + t1.pprint + " then " + t2.pprint + " else " + t3.pprint

  override def debug =
    "if " + t1.debug + " then " + t2.debug + " else " + t3.debug

  def toLatex(implicit o: IOptions): String = postProcess(
    s"\\mathsf{if}~${t1.toLatex}~\\mathsf{then}~${t2.toLatex}~\\mathsf{else}~${t3.toLatex}"
  )

  override val subTerms = Seq(t1, t2, t3)

  def derivationName = "TIf"
}

/** A derivation tree of the TYP-APP application rule
  * @param t1
  *   The function to apply
  * @param t2
  *   The argument to apply the function to
  */
case class IApp(t1: TypingDerivation, t2: TypingDerivation) extends ISerious {

  def tpe = {
    val t1tpe = t1.tpe
    val a1 = dom(t1tpe)
    val a2 = cod(t1tpe)

    if (t2.tpe == a1) a2
    else {
      throw new ITypeError(this, t2.tpe, a1)
    }
  }

  def pprint = t1.pprint + " " + t2.pprint

  override def debug = t1.debug + " " + t2.debug

  def toLatex(implicit o: IOptions): String = {

    postProcess("(" + t1.toLatex + t2.toLatex + ")")
  }

  override val subTerms = Seq(t1, t2)
  /*override def judgments(g: RuntimeEnvironment) = {
    val (xp, ty1, ty2, sep) = destr(g)
    Seq(Leq(t2.se(g).dotit(), xp.s))
  }*/

  def derivationName = "TApp"
}

/** A derivation tree of the annotation rule
  * @param t
  *   The term to annotate
  * @param t
  *   The type to annotate the term with
  */
case class IAsc(t: TypingDerivation, ty: Type, e: Evidence) extends ISerious {

  override def isValue: Boolean = t match {
    case IAsc(_, _, _) => false
    case _             => t.isValue
  }

  def tpe = {
    t.tpe
    if (TypeOps.consistent(t.tpe, ty)) ty
    else throw new ITypeError(t, t.tpe, ty)
  }

  def pprint = "(" + t.pprint + " :: " + ty.pprint + ")"

  override def debug = "(" + t.debug + " : " + ty.debug + ")"

  def toLatex(implicit o: IOptions): String = postProcess(
    // s"(${if (o.hideEvidences) "" else e.toLatex} ${t.toLatex} :: ${ty.toLatex})"
    if (isValue)
      s"${t.toLatex}_{${if (o.hideEvidences) "" else e.toLatex}}"
    else
      s"(${if (o.hideEvidences) "" else e.toLatex} ${t.toLatex})"
  )

  override val subTerms = Seq(t)

  override def judgments = Seq(Consistency(t.tpe, ty, e))

  def derivationName = "TAsc"
}
object IAsc {
  def apply(t: TypingDerivation, ty: Type) = {
    new IAsc(t, ty, EvidenceOps.initialEvidence(t.tpe, ty))
  }
}

/** A derivation tree of a let expression
  * @param x
  *   The variable to bind
  * @param t1
  *   The term to bind the variable to
  * @param t2
  *   The term to evaluate
  */
case class ILet(x: IVar, t1: TypingDerivation, t2: TypingDerivation)
    extends ISerious {
  def tpe = t2.tpe

  def pprint = s"let ${x.pprint} = ${t1.pprint} in ${t2.pprint}"

  override def debug = s"let ${x.debug} = ${t1.debug} in ${t2.debug}"

  def toLatex(implicit o: IOptions): String = {
    postProcess(
      s"\\mathsf{let}~{${x.toLatex}} = {${t1.toLatex}}~\\mathsf{in}~{${t2.toLatex}}"
    )
  }

  override val subTerms = Seq(t1, t2)

  def derivationName = "TLet"
}

/** A derivation tree of a first operation over a pair
  * @param t
  *   The term to evaluate
  */
case class IFirst(t: TypingDerivation) extends ISerious {
  def tpe = pi1(t.tpe)

  def pprint = "fst(" + t.pprint + ")"

  override def debug = "fst(" + t.debug + ")"

  def toLatex(implicit o: IOptions): String = postProcess(
    "\\fst^{" + "}(" + t.toLatex + ")"
  )

  override val subTerms = Seq(t)

  def derivationName = "TFst"
}

/** A derivation tree of a second operation over a pair
  * @param t
  *   The term to evaluate
  */
case class ISecond(t: TypingDerivation) extends ISerious {
  def tpe = pi2(t.tpe)

  def pprint = "snd(" + t.pprint + ")"

  override def debug = "snd(" + t.debug + ")"

  def toLatex(implicit o: IOptions): String = postProcess(
    "\\snd^{" + "}(" + t.toLatex + ")"
  )

  override val subTerms = Seq(t)

  def derivationName = "TSnd"
}

/** A derivation tree of a case expression
  * @param t
  *   The term to evaluate
  * @param x1
  *   The variable to bind the left branch to
  * @param t1
  *   The term to evaluate if the left branch is taken
  * @param x2
  *   The variable to bind the right branch to
  * @param t2
  *   The term to evaluate if the right branch is taken
  */
case class ICase(
    t: TypingDerivation,
    x1: IVar,
    t1: TypingDerivation,
    x2: IVar,
    t2: TypingDerivation
) extends ISerious {

  def tpe = {
    val ty1 = t1.tpe
    val ty2 = t2.tpe
    // println(ty1, ty2)
    if (ty1 == ty2) {
      ty1
    } else throw new Exception(ty1.pprint + " is not equal to " + ty2.pprint)
  }

  def pprint =
    s"case ${t.pprint} of {${x1.pprint} => ${t1.pprint}} {${x2.pprint} => ${t2.pprint}}"

  override def debug =
    s"case ${t.debug} of {${x1.debug} => ${t1.debug}} {${x2.debug} => ${t2.debug}}"

  def toLatex(implicit o: IOptions): String = {
    postProcess(
      s"\\mathsf{case}~${t.toLatex}~\\mathsf{of}~\\{${x1.toLatex} \\Rightarrow ${t1.toLatex}\\}~\\{${x2.toLatex} \\Rightarrow ${t2.toLatex}\\}"
    )
  }

  override val subTerms = Seq(t, t1, t2)

  def derivationName = "TCase"
}

/** A derivation tree of a lambda abstraction
  * @param x
  *   The variable to bind
  * @param t
  *   The term to bind the variable to
  * @param ty
  *   The type of the lambda
  */
case class ILambda(x: IVar, t: TypingDerivation) extends SimpleValue {

  def tpe = {
    FuncType(x.tpe, t.tpe)
  }

  def pprint = "(" + lambdaS.pprint + x.pprint + ". " + t.pprint + s")"

  override def debug = "(" + lambdaS.pprint + x.debug + ". " + t.debug + s")"

  def toLatex(implicit o: IOptions): String = postProcess(
    "(\\lambda " + x.toLatex + ". " + t.toLatex + s")"
  )

  override val subTerms = Seq(t)

  def derivationName = "TLm"

  override val mode = 2
}

/** A derivation tree of a fixpoint
  * @param x
  *   The variable to bind
  * @param t
  *   The term to bind the variable to
  * @param ty
  *   The type of the fixpoint
  */
case class IFix(x: IVar, t: TypingDerivation) extends ISerious {

  def tpe = {
    if (x.tpe == t.tpe) x.tpe
    else throw new ITypeError(t, t.tpe, x.tpe)
  }

  def pprint = "(fix " + x.pprint + ". " + t.pprint + s")"

  override def debug = "(fix " + x.debug + ". " + t.debug + s")"

  def toLatex(implicit o: IOptions): String = postProcess(
    s"\\mathsf{fix}~{${x.toLatex}}.{${t.toLatex}}"
  )

  override val subTerms = Seq(t)

  def derivationName = "TFix"

  override val mode = 2
}
