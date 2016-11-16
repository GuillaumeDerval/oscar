package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression
import oscar.modeling.algebra.integer.IntExpression

/**
 * Max of an array
 */
case class IntExpressionToFloat(from: IntExpression) extends FloatExpression {
  override def evaluate(): Double = from.evaluate()
  override def min: Double = from.min
  override def max: Double = from.max

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[Expression] = from.subexpressions()

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = IntExpressionToFloat(func(from).asInstanceOf[IntExpression])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = false

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = false
}
