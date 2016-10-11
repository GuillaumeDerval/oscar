package algebra

import scala.collection.mutable.HashSet

/**
 * Expression left-right
 * @param left left-hand of the expression, to which ``right`` will be subtracted
 * @param right right-hand of the expression
 */
case class Minus(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() - right.evaluate()
  override def min: Int = left.min - right.max
  override def max: Int = left.max - right.min
  override def values(): Iterable[Int] = {
    val s = new HashSet[Int]()
    for(i <- left.values)
      for(j <- right.values)
        s += i-j
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
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Minus(func(left), func(right))
}
