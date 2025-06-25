package lang.runtime.simple

import lang.runtime.{IRuntimeException, LinearEnv}
import lang.runtime.SubstitutionModel.SubstitutionModel
import lang.typing.{
  Boxeable,
  Highlighteable,
  IConfLatex,
  IOptions,
  TypingDerivation
}
import lang.runtime.simple.Ops.*

/** The Trampoline trait has three implementations: A Done state meaning that we
  * are done. A More state meaning that we took an intermediate step but we
  * should not pay attention to it. A Step state meaning that we took a step and
  * we must pay attention and log it to the interface
  */
sealed trait Trampoline {

  /** This function is the main function of the trampoline. It is called by the
    * interface to take a step in the evaluation of the term. It returns a
    * Result object that contains the result of the evaluation and the
    * configurations that were generated during the evaluation.
    */
  def step(implicit o: IOptions): Result = {
    this match {
      /** if we are done we append the current configuration to the list of
        * configurations and return a Result.
        */
      case Done(reducer, a) => {
        reducer.appendLatex(reducer.current)
        Result(Left(a), reducer.configurations, reducer.nstep, true)
      }

      /** We took a step but we shouldn't stop. Let us continue reducing... */
      case More(t) => t().step

      /** We took an important step of reduction. Lets take a breath, increase
        * the number of steps and log the current configuration. We also mark
        * with a box the redex of the type derivation.
        */
      case s @ Step(reducer, lstack1, lenv1, lstack2, lenv2, t, _, _) => {
        reducer.nstep += 1
        implicit val substitutionMode = s.substitutionMode
        val iterm = reconstructITerm(lstack1)
        reducer.appendLatex(
          Configuration(IContext(iterm, lstack1(0).env), lenv1)
        )
        reducer.current = Configuration(
          IContext(reconstructITerm(lstack2), lstack2(0).env),
          lenv2
        )
        if (reducer.nstep < reducer.fromStep + reducer.stepSize) {
          lstack2.map {
            _.t.unboxLevel
          }
          t(lstack2).step
        } else {
          Result(Left(iterm), reducer.configurations, reducer.nstep, false)
        }
      }

      /** In case we got an error we just stop. Similar to the Done step. */
      case IError(reducer, lstack, lenv1, error, toBoxExtra) => {
        val iterm = reconstructITerm(lstack)
        reducer.appendLatex(
          Configuration(IContext(iterm, lstack(0).env), lenv1)
        )
        Result(Right(error), reducer.configurations, reducer.nstep, true)
      }

    }
  }
}

/** This class represents the final state of the evaluation. It contains the
  * result of the evaluation.
  *
  * @param a
  */
case class Done(
    reducer: SimpleReducer,
    a: TypingDerivation
) extends Trampoline

/** This class represents a state where we took a step but we should not stop.
  *
  * @param a
  *   A continuation to keep reducing
  */
case class More(a: () => Trampoline) extends Trampoline

/** This class represents a state where we took a step and we should stop. It
  * contains the new frames, the new linear environment, the new continuation,
  * the elements to highlight and the elements to box.
  *
  * @param lstack1
  *   The frames before the steps
  * @param lenv1
  *   The linear environment before the step
  * @param lstack2
  *   The frames after the step
  * @param lenv2
  *   The linear environment after the step
  * @param a
  *   The continuation
  * @param toHighlight
  *   The elements to highlight
  * @param toBoxExtra
  *   The elements to box
  * @param substitutionMode
  *   The substitution mode
  */
case class Step(
    reducer: SimpleReducer,
    lstack1: Frames,
    lenv1: LinearEnv,
    lstack2: Frames,
    lenv2: LinearEnv,
    a: Frames => Trampoline,
    toHighlight: List[Highlighteable] = Nil,
    toBoxExtra: List[Boxeable] = Nil
)(implicit val substitutionMode: SubstitutionModel)
    extends Trampoline {
  toHighlight.map {
    _.startHighlightTimer
  }
  (toBoxExtra ++ lstack1.headOption.map { s => List(s.t) }.getOrElse(Nil))
    .map { boxeable =>
      boxeable.startBoxedTimer
    }
}

/** This class represents an error in the evaluation of the program. It stores
  * many things beside the error itself.
  *
  * @param lstack
  *   the current frames
  * @param lenv1
  *   the current linear environment
  * @param error
  *   the error
  * @param toBoxExtra
  *   a list of extra elements to be boxed for the latex output
  * @param substitutionMode
  *   the substitution mode
  */
case class IError(
    reducer: SimpleReducer,
    lstack: Frames,
    lenv1: LinearEnv,
    error: IRuntimeException,
    toBoxExtra: List[Boxeable] = Nil
)(implicit val substitutionMode: SubstitutionModel)
    extends Trampoline {
  (toBoxExtra ++ lstack.headOption.map { s => List(s.t) }.getOrElse(Nil))
    .map { boxeable =>
      boxeable.startBoxedTimer
    }
}
