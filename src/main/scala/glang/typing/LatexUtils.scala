package glang.typing

import glang.runtime.RuntimeEnvironment

/** Trait to paint something of a different color in the latex representation */
trait Highlighteable {
  var highlight: Int = 0
  def startHighlightTimer = {
    highlight = 2
  }
  def unhighlightLevel: Unit = {
    if (highlight > 0) highlight -= 1
  }
}

/** Trait to draw a box around some expressions in the latex representation */
trait Boxeable {
  var boxed: Int = 0
  def startBoxedTimer = boxed = 1

  def unboxLevel: Unit = {
    if (boxed > 0) boxed -= 1
  }
  def boxLevel: Unit = {
    boxed += 1
  }
}

/** Class to represent a derivation tree in the frontend
  * @param term
  *   The term of the derivation of the current node in latex format
  * @param subtrees
  *   The subtrees of the derivation
  * @param judgments
  *   The judgments of the derivation
  * @param name
  *   The name of the derivation that matches the name of the rule
  */
case class LatexDerivationTree(
    term: String,
    subtrees: List[LatexDerivationTree],
    judgments: List[LatexJudgment],
    name: String = ""
)

/** Class to represent a judgment in the frontend
  * @param judgment
  *   The judgment in latex format
  */
case class LatexJudgment(judgment: String)

/** Class to represent a configuration for the latex representation
  * @param tree
  *   The derivation tree
  * @param env
  *   The environment
  * @param store
  *   The store
  */
case class IConfLatex(tree: LatexDerivationTree, env: String, store: String)

/** Trait to represent something that have a latex representation
  */
trait Latexable {

  /** Method to convert the object to latex
    * @param o
    *   The options to use in the conversion
    * @return
    *   The latex representation of the object
    */
  def toLatex(implicit o: IOptions): String

}

/** Class to represent options for the latex generation of a term. It can be
  * extended with extra parameters from the interface
  * @param g
  *   The runtime environment
  */
case class IOptions(
    hideEvidences: Boolean = false,
    g: RuntimeEnvironment = new RuntimeEnvironment(),
    hideBoxes: Boolean = false
)
