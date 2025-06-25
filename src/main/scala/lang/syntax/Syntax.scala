package lang.syntax

/** Object that contains various common elements from the syntax and types.
  */
object Syntax {

  /** Case class representing a symbol with a parser and pretty-print string.
    * This is used by the parser to convert strings from javascript to something
    * understandable by the parsing library.
    *
    * @param parser
    *   The string used by the parser.
    * @param pprint
    *   The pretty-print string.
    */
  case class Symbol(parser: String, pprint: String)

  // Defining several symbols used in the language
  val lambdaS = Symbol("@", "λ") // "λ"
  val andS = Symbol("∧", "∧")
  val orS = Symbol("∨", "∨")
  val notS = Symbol("¬", "¬")
  val ltS = Symbol("<", "<")
  val gtS = Symbol(">", ">")
  val eqS = Symbol("==", "==")
  val timesS = Symbol("*", "*")
  val minusS = Symbol("-", "-")
  val plusS = Symbol("+", "+")
  val inf = Symbol("∞", "∞")

  // List of symbols availabe for the parser
  val symbols = List(lambdaS, andS, orS, notS, ltS, gtS, eqS, timesS, inf)

  /** Trait for objects that can be pretty-printed. th debug function may be
    * used to change the way objects are printed
    */
  trait Printeable {
    def pprint: String

    def debug: String = pprint
  }

  /** Case class representing a simple type such as Int or Bool.
    */
  trait SimpleType

  /** Trait for terms that can be printed.
    */
  trait Term extends Printeable {}



}
