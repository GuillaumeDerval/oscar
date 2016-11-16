package oscar.modeling.algebra.integer

/**
 * Expression of -value
 *
 * @param value the value of the constant
 */
case class UnaryMinus(val value: IntExpression) extends IntExpression {
  override def evaluate(): Int = -value.evaluate()
  override def min: Int = -value.max
  override def max: Int = -value.min
  override def values(): Iterable[Int] = value.values.map(-_)

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array(value)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new UnaryMinus(func(value))
}
