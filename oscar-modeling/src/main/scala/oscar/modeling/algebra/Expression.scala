package oscar.modeling.algebra

/**
  * An expression, that can be floating, integer or boolean
  */
trait Expression {
  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  def subexpressions(): Iterable[Expression]

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  def mapSubexpressions(func: (Expression => Expression)): Expression
}
