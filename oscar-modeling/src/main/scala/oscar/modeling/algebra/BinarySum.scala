package oscar.modeling.algebra

import scala.collection.mutable.HashSet

/**
 * Expression left+right
 * @param left left-hand of the sum
 * @param right right-hand of the sum
 */
case class BinarySum(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() + right.evaluate()
  override def min: Int = left.min + right.min
  override def max: Int = left.max + right.max
  override def values(): Iterable[Int] = {
    val s = new HashSet[Int]()
    for(i <- left.values)
      for(j <- right.values)
        s += i+j
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
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new BinarySum(func(left), func(right))
}
