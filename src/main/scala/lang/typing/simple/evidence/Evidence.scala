package lang.typing.simple.evidence

import lang.syntax.Type
import lang.typing.{Boxeable, Highlighteable, IOptions}
import lang.syntax.simple.TypeOps.*
import EvidenceOps.*

/** evidence stuff */
type EType = Type
case class Evidence(l: EType, r: EType) extends Highlighteable with Boxeable {

  def toLatex(implicit o: IOptions): String = {

    val text = {
      s"\\langle ${l.toLatex}, ${r.toLatex} \\rangle"
    }
    val htext = if (highlight == 1) s"\\highlight{$text}" else text
    if (boxed == 1 && !o.hideBoxes) s"\\bbox[1px,border:2px solid red]{$htext}"
    else htext
  }

  override def toString: String = toLatex(IOptions())

  def pprint: String = {
    "<" + l.pprint + "," + r.pprint + ">"
  }

}
