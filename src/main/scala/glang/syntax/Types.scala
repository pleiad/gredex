package glang.syntax
import Syntax.*
import glang.typing.Latexable
import java.awt.print.Printable

/** Trait for general types
  */
trait Type extends Printeable {

  def toLatex: String

  
}





/** Class to represent a hole in a type. It is used to type Hole Terms.
  */
case class HoleType() extends Type {
  def pprint = "[]"

  def toLatex = "[]"

}

case class TypeEnvironment(m: Map[Var, Type] = Map()) {
  def lookup(x: Var) = m.find(_._1.x == x.x).map(_._2)

  def extend(x: Var, t: Type): TypeEnvironment = new TypeEnvironment(
    m + (x -> t)
  )

  def extend(t: Tuple2[Var, Type]): TypeEnvironment = new TypeEnvironment(
    m + t
  )
}

case class Environments(te: TypeEnvironment = TypeEnvironment()) {
  def extend(x: Var, t: Type) = this.copy(te = this.te.extend(x, t))

  def extend(vars: Tuple2[Var, Type]) = this.copy(te = this.te.extend(vars))
}

object TypeEnvironment {
  def apply(vars: Tuple2[Var, Type]*) = new TypeEnvironment(vars.toMap)
}
