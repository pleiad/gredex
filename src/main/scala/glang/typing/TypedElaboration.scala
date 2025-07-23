package glang.typing

import glang.syntax.Syntax._
import glang.syntax._

/** Type checking algorithm. This object contains the implementation of the
  * elaboration algorithm that builds a type derivation tree from an AST. This
  * algorithm *could* inserts casts or some needed annotations in the program to
  * remove non-determinism of typing. For instance (\x. x) is ambiguous, but
  * (\x. x : T -> T) is not.
  */
trait TypedElaboration {

  

  /** Creates a frame with a term, environment, and typing mode (either check or
    * infer).
    *
    * @param t
    *   The term.
    * @param env
    *   The environment.
    * @param mode
    *   The type direction.
    * @return
    *   A new Frame instance.
    */
  def c(t: Term, env: Environments, mode: TypeDirection = Infer) =
    Frame(t, env, mode)

  /** Applies a term and environment to create an type derivation.
    *
    * @param t
    *   The term.
    * @param env
    *   The environment.
    * @return
    *   A type derivation term
    */
  def apply(t: Term, env: Environments = Environments()): TypingDerivation = {
    val res = apply(List(Frame(t, env, Infer)), List())
    res.tpe // make sure it can be typed
    res
  }

  /** Trait representing the direction of type checking.
    */
  trait TypeDirection {
    def pprint: String
  }

  /** Case object representing inference mode.
    */
  case object Infer extends TypeDirection {
    def pprint = "=>"
  }

  /** Case class representing check mode, checking against a specific type.
    *
    * @param ty
    *   The type to check against.
    */
  case class Check(ty: Type) extends TypeDirection {
    def pprint = "<="
  }

  /** Case class representing a frame of a term with an environment and type
    * direction.
    *
    * @param t
    *   The term.
    * @param env
    *   The environment.
    * @param mode
    *   The type direction.
    */
  case class Frame(t: Term, env: Environments, mode: TypeDirection = Infer) {
    def pprint = t.pprint
  }

  /** Convert a stack of frames into a stack of type derivations. Initially the
    * stack is full and istack is empty. The algorithm ends when stack is empty.
    *
    * @param stack
    *   The list of frames. The input of the algorithm
    * @param istack
    *   The list of type derivations. The output of the algorithm
    * @return
    *   An type derivation.
    */
  def apply(
      stack: List[Frame],
      istack: List[TypingDerivation]
  ): TypingDerivation
}
