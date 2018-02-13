package oscar.xcsp3


/**
  * Thrown when a solution given for a XCSP3 is invalid
  */
class XCSP3ValidationException extends Exception {}

/**
  * Thrown when a solver running a XCSP3 model does a timeout
  */
class XCSP3TimeoutException extends Exception {}

/**
  * Thrown when the parse received an invalid input (not respecting the XCSP3 language)
  */
class XCSP3ParseException(message: String) extends Exception(message) {}