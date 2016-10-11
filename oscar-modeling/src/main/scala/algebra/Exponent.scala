package algebra

import scala.collection.mutable.HashSet

/**
 * Expression for pow(base,exponent)
 * @param base the base of the exponential
 * @param exponent the exponent
 */
case class Exponent(val base: IntExpression, val exponent: IntExpression) extends IntExpression {
  override def evaluate(): Int = Math.pow(base.evaluate(), exponent.evaluate()).toInt
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
    for(i <- base.values)
      for(j <- exponent.values)
        s += Math.pow(i,j).toInt
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array(base, exponent)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Exponent(func(base), func(exponent))
}
