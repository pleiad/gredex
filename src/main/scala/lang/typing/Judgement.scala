package lang.typing

import lang.syntax.Type

/** This trait represent a judgment. For now we only have subtyping but this can
  * be extended with any other judgment
  */
trait Judgement {

  /** the latex string representation of the relation */
  val op: String

  /** Convert the judgment to latex
    *
    * @param o
    *   The options to use in the conversion
    * @return
    *   The latex representation of the judgment
    */
  def toLatex(implicit o: IOptions): String
}
