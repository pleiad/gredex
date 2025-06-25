package lang.runtime

import lang.syntax.Syntax.Term
import lang.typing.{IConfLatex, IVar, TypingDerivation}

/** this object defines the different substitution models you can adopt. In this
  * implementation we explicit substitution but you can support both models
  */
object SubstitutionModel extends Enumeration {
  type SubstitutionModel = Value
  val Explicit, Implicit = Value
}

/** The type of the RuntimeEnvironment. Adapt this to any kind of environment
  * you want to track during runtime. For instance, now is used to support
  * implicit substitutions.
  */
type RuntimeEnvironment = Env[IVar]

/** This trait represents a result of a reduction. It contains the result of the
  * reduction, the configurations of the reduction, the step where the reduction
  * stopped and a flag indicating if the reduction is finished.
  */
trait BaseResult {
  val r: Either[Term, IRuntimeException]
  val configurations: List[IConfLatex]
  val step: Int
  val finished: Boolean
}
