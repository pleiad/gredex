package glang.typing

import glang.syntax.simple.TypeOps.*
import glang.syntax.Syntax.*
import glang.syntax.*
import glang.runtime.RuntimeEnvironment
import glang.runtime.IRuntimeException
import glang.runtime.IResult
import glang.typing.Latexable
import glang.typing.simple.IAsc

trait TypingDerivation
    extends Term
    with Highlighteable
    with Boxeable
    with Latexable
    with IResult {

  def isValue = false

  /** Every type derivation has a type */
  def tpe: Type

  /** Infer mode or check mode (1 for infer, 2 for check)
    */
  val mode: Int = 1

  /** Function to do a post-processing of the string returning it for latex
    * printing
    * @param s
    *   The string to process
    * @param o
    *   The options to use in the conversion
    * @return
    *   The processed string
    */
  def postProcess(s: String)(implicit o: IOptions): String = {
    val text = s
    /* highlighting and boxing of text */
    val htext = if (highlight == 1) s"\\highlight{$text}" else text
    if (boxed == 1 && !o.hideBoxes) s"\\underline{$htext}" //  bbox[1px,border:2px solid blue]
    else htext
  }


  /** The subterms of the derivation By default they are empty
    */
  val subTerms: Seq[TypingDerivation] = Nil

  /** The judgments of the derivation. By default empty
    * @param g
    *   The runtime environment
    * @return
    *   The judgments of the derivation
    */
  def judgments: Seq[Judgement] = Nil

  /** The name of the derivation
    */
  def derivationName: String

  /** Collect all the judgments of the child synth asc nodes recursively
    * @param t
    * @return
    */
  def collectChildSynthJudgments(
      t: TypingDerivation
  )(implicit o: IOptions): List[LatexJudgment] = {
    t.subTerms.flatMap {
      case a: IAsc if a.synth =>
        a.judgments
          .filter(c => !c.isReflexive())
          .map { j =>
            LatexJudgment(j.toLatex)
          }
          .toList ++ collectChildSynthJudgments(a)
      case other => List()
    }.toList

  }

  /** Get the latex representation of the term
    * @param o
    *   The options to use in the conversion
    * @return
    */
  def getLatexDerivationTree(implicit o: IOptions): LatexDerivationTree = {
    this match {
      case v: IAsc if o.hideSynthAsc && v.synth => v.t.getLatexDerivationTree
      case _ =>
        val extraJudgments = collectChildSynthJudgments(this)
        LatexDerivationTree(
          toLatex + s" : ${tpe.toLatex}",
          subTerms.map { t =>
            t.getLatexDerivationTree(o.copy(hideBoxes = true))
          }.toList,
          judgments.map { j =>
            LatexJudgment(j.toLatex)
          }.toList ++ extraJudgments,
          derivationName
        )
    }

  }

  /** decrease the highlight counter by one */
  override def unhighlightLevel: Unit = {
    highlight -= 1
    subTerms.map {
      _.unhighlightLevel
    }
  }

  /** decrease the box level by one */
  override def unboxLevel: Unit = {
    if (boxed > 0) boxed -= 1
    subTerms.map {
      _.unboxLevel
    }
  }

  /** increase the box level by one */
  override def boxLevel: Unit = {
    boxed += 1
    subTerms.map {
      _.boxLevel
    }
  }

}

/** values * */

/** This trait represents any tree derivation of a value on runtime
  */
trait IValue extends TypingDerivation {
  override def isValue = true
}

/** A simple value is something that is not a function */
trait SimpleValue extends IValue

/** A serious expression is a redex */
trait ISerious extends TypingDerivation

/** A derivation tree of a variable
  * @param x
  *   The name of the variable
  * @param ty
  *   The type of the variable
  */
case class IVar(x: String, ty: Type) extends ISerious {

  def tpe = ty

  def pprint = x.toString

  def toLatex(implicit o: IOptions): String = postProcess(
    "\\texttt{" + pprint + s"}"
  )

  def derivationName = "TVAR"
}

/** Tag to mark a subtree as in Checked mode */
trait IChecked extends TypingDerivation

/** Tag to mark a serious subtree as in Checked mode */
trait ICheckedSerious extends IChecked

/** Tag to mark a value subtree as in Checked mode */
trait ICheckedValue extends IChecked

/** A derivation tree of a hole
  */
case class IHole() extends TypingDerivation {
  def tpe = HoleType()

  def pprint = "[]"

  def toLatex(implicit o: IOptions): String = "[]"

  def derivationName = "E[]"
}
