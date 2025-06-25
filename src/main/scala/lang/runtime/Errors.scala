package lang.runtime

/** This class represents a runtime exception * */
class IRuntimeException(val s: String) extends Exception(s) with IResult
