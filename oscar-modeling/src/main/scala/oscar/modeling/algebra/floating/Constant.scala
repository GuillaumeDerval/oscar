package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression

/**
 * Expression of a constant
  *
  * @param value the value of the constant
 */
case class Constant(value: Double) extends FloatExpression {
  override def evaluate(): Double = value
  override def min: Double = value
  override def max: Double = value

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = Array[FloatExpression]()

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = this

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = true

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = true

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = true
}