package oscar.modeling.algebra

import scala.collection.mutable.HashSet

/**
 * Expression left*right
 * @param left left-hand of the product
 * @param right right-hand of the product
 */
case class Prod(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() * right.evaluate()
  override def min: Int = Math.min(Math.min(left.min*right.min, left.min*right.max), Math.min(left.max*right.min, left.max*right.max))
  override def max: Int = Math.max(Math.max(left.min*right.min, left.min*right.max), Math.max(left.max*right.min, left.max*right.max))
  override def values(): Iterable[Int] = {
    val s = new HashSet[Int]()
    for(i <- left.values)
      for(j <- right.values)
        s += i*j
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
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Prod(func(left), func(right))
}