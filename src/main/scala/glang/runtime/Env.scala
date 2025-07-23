package glang.runtime

import glang.typing._
import scala.collection.immutable.ListMap

/** This class represents an environment of T to IValues. This class is
  * exclusive to support implicit substitutions.
  * @param m
  *   The map of the environment
  * @tparam T
  *   The type of the keys in the map
  */
class Env[T <: TypingDerivation](
    val m: ListMap[T, IValue] = ListMap[T, IValue]()
) {

  /** Lookup a value in the environment
    * @param x
    *   The key to lookup
    * @return
    *   The value associated with the key
    */
  def lookup(x: T) = m.get(x)

  /** Extend the environment with a new value
    * @param x
    *   The key to add
    * @param t
    *   The value to add
    * @return
    *   The new environment
    */
  def extend(x: T, t: IValue): Env[T] = new Env[T](m + (x -> t))

  /** Extend the environment with a new value
    * @param t
    *   The tuple to add
    * @return
    *   The new environment
    */
  def extend(t: Tuple2[T, IValue]): Env[T] = new Env[T](m + t)

  /** Combine two environments
    * @param env2
    *   The environment to combine
    * @return
    *   The new environment
    */
  def extend(env2: Env[T]): Env[T] = new Env[T](m ++ env2.m)

  /** Pretty print the environment
    * @return
    *   The pretty printed environment
    */
  def pprint = {
    "[" + m.map { case (t, v) => t.pprint }.mkString(", ") + "]"
  }

  /** Convert the environment to latex
    * @param o
    *   The options to use in the conversion
    * @return
    *   The latex representation of the environment
    */
  def toLatex(implicit o: IOptions) = "[" + m
    .map { case (t, v) => t.toLatex + " \\mapsto " + v.toLatex }
    .mkString(", ") + "]"

  /** Project the environment to a new environment with only the value of x
    * @param x
    *   The key to project
    * @return
    *   The new environment
    */
  def project(x: T) = m.get(x).map { v => new Env[T](ListMap(x -> v)) }

  /** Remove several keys from the environment
    * @param env2
    *   The environment with the keys to remove
    * @return
    *   The new environment
    */
  def -(env2: Env[T]): Env[T] = m.foldLeft(new Env[T]) {
    case (acc, (x, v)) => {
      if (!env2.lookup(x).isDefined)
        acc.extend(x -> v)
      else acc
    }
  }
}

/** This class represents a linear environment. Linear environments in the sense
  * that each instructions could modify it, contrary to the runtime environment
  * which is immutable and has a lexical scope. For instance you can use this
  * class to track the HEAP in the presence of mutable references. For now it
  * doesn't do anything. Adapt this to any kind of environment you want to track
  * during runtime.
  */
case class LinearEnv() {
  def toLatex = "[]"
}
