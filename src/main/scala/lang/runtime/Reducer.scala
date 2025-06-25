package lang.runtime

/** this trait defines a reducer of type derivations. To create another reducer, extends this interface
 * and implement the reduce method.
 *
 * @tparam T The type of the result of the reduction
 */
trait Reducer[T <: BaseResult] {
  def reduce: BaseResult
}
