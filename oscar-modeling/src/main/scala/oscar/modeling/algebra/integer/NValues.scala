package oscar.modeling.algebra.integer

import oscar.modeling.algebra.Expression

/**
 * Number of different values in a given array of IntExpression
 */
case class NValues(a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.map(_.evaluate()).toSet.size
  override def min: Int = 1
  override def max: Int = a.length
  override def values(): Iterable[Int] = min to max

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = NValues(a.map(func).asInstanceOf[Array[IntExpression]])
}
