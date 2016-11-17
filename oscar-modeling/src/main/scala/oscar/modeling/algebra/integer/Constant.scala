package oscar.modeling.algebra.integer

import oscar.modeling.algebra.Expression

/**
 * Expression of a constant
  *
  * @param value the value of the constant
 */
case class Constant(value: Int) extends IntExpression {
  override def evaluate(): Int = value
  override def min: Int = value
  override def max: Int = value
  override def values(): Iterable[Int] = List(value)

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array[IntExpression]()

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = this

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = true
}