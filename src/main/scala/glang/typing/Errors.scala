package glang.typing

import glang.runtime.IRuntimeException
import glang.syntax.Syntax.Term
import glang.syntax.Type

/** Two cases of errors: Type error and Ambiguity errors */
case class IError(msg: String = "")
    extends IRuntimeException(s"Type error! ${msg}")

/** Exception thrown when a variable cannot be found.
  *
  * @param t
  *   The term that caused the exception.
  */
class VariableNotFoundException(t: Term)
    extends Error("Can't find type for variable " + t.pprint)

/** Exception thrown for invalid subtyping relations.
  *
  * @param t1
  *   The first type in the subtyping relation.
  * @param t2
  *   The second type in the subtyping relation.
  */
class ITypeError(t: Term, found: Type, required: Type)
    extends Error(
      "Type mismatch in \"" + t.pprint + "\": Found " + found.pprint + ", required " + required.pprint + "."
    )

/** Exception thrown when a type cannot be found for an operation.
  *
  * @param op
  *   The operation that caused the exception.
  */
class ITypeNotFound(op: String)
    extends Error(s"Type not found for operation ${op}")

/** Exception thrown when a type is not compatible with another type.
  *
  * @param t1
  *   The first type *
  * @param t2
  *   The second type
  */
class IMeetError(t1: Type, t2: Type)
    extends IRuntimeException(
      "Meet error: " + t1.pprint + " and " + t2.pprint + " are not compatible."
    )

/** Exception thrown when the interior is not defined
  *
  * @param t1
  *   The first type
  * @param t2
  *   The second type
  */
class IInteriorError(t1: Type, t2: Type)
    extends IRuntimeException(
      "Interior error: " + t1.pprint + " and " + t2.pprint + " is not defined."
    )

/** Exception thrown for stack errors.
  */
class StackError extends Error("stack error!")
