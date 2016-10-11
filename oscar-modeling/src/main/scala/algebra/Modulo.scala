package algebra

import scala.collection.mutable.HashSet

/**
 * Expression left%right (euclidian division reminder)
 * @param left The numerator
 * @param right The denominator
 */
case class Modulo(val left: IntExpression, val right: Int) extends IntExpression {
  override def evaluate(): Int = left.evaluate() % right
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
      s += i%right
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array(left)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Modulo(func(left), right)
}
