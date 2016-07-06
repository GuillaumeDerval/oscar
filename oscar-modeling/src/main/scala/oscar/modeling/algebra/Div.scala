package oscar.modeling.algebra

import scala.collection.mutable.HashSet

/**
 * Expression left/right (euclidian division)
 * @param left The numerator
 * @param right The denoinator
 */
case class Div(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() / right.evaluate()
  override def min: Int = {
    //TODO: we can make it better easily
    values.min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values.max
  }
  override def values(): Iterable[Int] = {
    val s = new HashSet[Int]()
    for(i <- left.values)
      for(j <- right.values)
        s += i/j
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array(left, right)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Div(func(left), func(right))
}
