package lang.runtime.simple
import lang.runtime.SubstitutionModel.{Explicit, SubstitutionModel}
import lang.runtime.{
  BaseResult,
  Env,
  IRuntimeException,
  LinearEnv,
  Reducer,
  RuntimeEnvironment
}
import lang.typing.{
  Boxeable,
  Highlighteable,
  IConfLatex,
  IHole,
  IOptions,
  IValue,
  IVar,
  TypingDerivation
}
import lang.typing.simple.*
import lang.runtime.simple.Ops.*
import lang.runtime.simple.handlers.*

import scala.annotation.tailrec

/** This class represents a frame of the reduction stack */
case class IContext(t: TypingDerivation, env: RuntimeEnvironment) {
  def pprint = {
    t.pprint + "  |   " + env.pprint

  }
}

type Frame = IContext
type Frames = List[Frame]

case class Configuration(c: Frame, lenv: LinearEnv)

/** Main class of reduction. This is the one in charge of reducing a type
  * derivation
  *
  * @param t
  *   The type derivation to reduce
  * @param env
  *   The lexical environment to use during reduction
  * @param lenv
  *   The linear environment to use during reduction
  * @param fromStep
  *   The step where the reduction starts (this in the case of many steps that
  *   may bloat the interface)
  * @param stepSize
  *   The number of single steps to take
  * @param substitutionMode
  *   The substitution model to use
  * @param o
  *   The options to use during reduction
  */
case class SimpleReducer(
    t: TypingDerivation,
    env: RuntimeEnvironment = new RuntimeEnvironment(),
    lenv: LinearEnv = new LinearEnv(),
    fromStep: Int = 0,
    stepSize: Int = 20
)(implicit
    substitutionMode: SubstitutionModel = Explicit,
    o: IOptions = IOptions()
) extends Reducer[Result] {

  /** the current step number */
  var nstep: Int = 0

  /** the list of type derivations and environments reduced so far */
  var configurations: List[IConfLatex] = Nil

  /** the current type derivation and environments */
  var current: Configuration = Configuration(IContext(t, env), lenv)

  def reduce: Result = {

    /** Initially the frame is just the complete type derivation and an empty
      * environment
      */
    val lstack = List(IContext(t, env))

    /** The first configuration is the initial one. We compute the latex
      * representation of the type derivatio tree to use it in the interface
      */
    configurations = List(
      IConfLatex(
        reconstructITerm(lstack).getLatexDerivationTree,
        env.toLatex,
        lenv.toLatex
      )
    )

    /** this is the call to the recursive function */
    reduce(lstack, lenv).step
  }

  /** This is the main function of the reduction. It takes a list of frames and
    * a linear environment and reduces the type derivation. Contrary to typing
    * here we only make use of one stack. It returns a trampoline to stop
    * reduction after a step, export to latex the current step, and continue
    * with the reduction. We also check if the number of steps have been reached
    * and if so we stop.
    *
    * @param lstack
    *   The current list of frames to reduce
    * @param lenv
    *   The current linear environment
    * @param substitutionMode
    *   The substitution model to use (here we are using explicit substitution,
    *   i.e. we are not using this paramter)
    * @return
    *   A trampoline to continue the reduction
    */
  implicit def reduce(lstack: Frames, lenv: LinearEnv)(implicit
      substitutionMode: SubstitutionModel
  ): Trampoline = {
    // print("Reducing: " + lstack.head.pprint + "\n")
    // print("Reconstructed term: " + reconstructITerm(lstack).pprint + "\n")

    OpsHandler(this).cases
      .orElse(BooleanHandler(this).cases)
      .orElse(FunctionHandler(this).cases)
      .orElse(AscriptionHandler(this).cases)
      .orElse(FixHandler(this).cases)
      .orElse(LetHandler(this).cases)
      .orElse(PairHandler(this).cases)
      .orElse(SumHandler(this).cases)
      .applyOrElse(
        (lstack, lenv),
        (lstack, lenv) =>
          (lstack, lenv) match { case (c :: Nil, _) => Done(this, c.t) }
      )

  }

  /** add the configuration to the list of configurations so far for latex
    * logging
    */
  def appendLatex(conf: Configuration): Unit = {
    conf.c.t.getLatexDerivationTree
    conf.c.env.toLatex
    conf.lenv.toLatex
    configurations = configurations :+ IConfLatex(
      conf.c.t.getLatexDerivationTree,
      conf.c.env.toLatex,
      conf.lenv.toLatex
    )
  }

}
