package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression

/**
 * Expression left-right
 *
 * @param left left-hand of the expression, to which ``right`` will be subtracted
 * @param right right-hand of the expression
  */
case class Minus(left: FloatExpression, right: FloatExpression) extends FloatExpression {
  override def evaluate(): Double = left.evaluate() - right.evaluate()
  override def min: Double = left.min - right.max
  override def max: Double = left.max - right.min

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = Array(left, right)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = Minus(func(left).asInstanceOf[FloatExpression], func(right).asInstanceOf[FloatExpression])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = subexpressions().forall(_.continuous)

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = subexpressions().forall(_.linear)
}
