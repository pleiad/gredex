package lang.syntax

/** Custom error class for type errors.
  *
  * @param s
  *   The error message.
  */
class TypeError(s: String = "Type Error") extends Error(s)
