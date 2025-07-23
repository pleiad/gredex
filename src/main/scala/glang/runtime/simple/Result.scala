package glang.runtime.simple

import glang.runtime.{BaseResult, IRuntimeException}
import glang.typing.{IConfLatex, TypingDerivation}

/** This class represents a result of a reduction. It contains the result of the
  * reduction, the configurations of the reduction, the step where the reduction
  * stopped and a flag indicating if the reduction is finished.
  */
case class Result(
    r: Either[TypingDerivation, IRuntimeException],
    configurations: List[IConfLatex],
    step: Int = 0,
    finished: Boolean = true
) extends BaseResult
