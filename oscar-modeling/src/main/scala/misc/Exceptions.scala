package misc

/**
 * Exception launched when an invalid CPVar is used to implement a Var
 * @param msg: an explanation message
 */
class InvalidCPVarException(msg: String) extends IllegalArgumentException(msg)

class EmptyDomainException extends Exception("")

class CannotBecomeSparseException extends Exception("")

class VariableNotBoundException extends Exception("")

class InvalidValueException extends Exception("")