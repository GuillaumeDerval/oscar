package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression

/**
 * Expression left/right
 *
 * @param left The numerator
 * @param right The denoinator
 */
case class Div(left: FloatExpression, right: FloatExpression) extends FloatExpression {
  override def evaluate(): Double = left.evaluate() / right.evaluate()

  private[this] def getPossibleBounds: Array[Double] = {
    val m1 = if(right.min != 0.0) Array(left.min/right.min, left.max/right.min) else Array()
    val m2 = if(right.max != 0.0) Array(left.min/right.max, left.max/right.max) else Array()
    m1 ++ m2
  }
  override def min: Double = getPossibleBounds.min
  override def max: Double = getPossibleBounds.max

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = Array(left, right)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = Div(func(left).asInstanceOf[FloatExpression], func(right).asInstanceOf[FloatExpression])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = subexpressions().forall(_.continuous)

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = false
}
