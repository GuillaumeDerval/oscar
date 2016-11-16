package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression

import scala.collection.mutable.HashSet

/**
 * Min of an array
 */
case class Min(a: Array[FloatExpression]) extends FloatExpression {
  override def evaluate(): Double = a.foldLeft(Double.MaxValue)((aa: Double, ba: FloatExpression) => aa min ba.evaluate())
  override def min: Double = a.foldLeft(Double.MaxValue)((aa: Double, ba: FloatExpression) => aa min ba.min)
  override def max: Double = a.foldLeft(Double.MaxValue)((aa: Double, ba: FloatExpression) => aa max ba.max)

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[FloatExpression] = a

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = Max(a.map(func).asInstanceOf[Array[FloatExpression]])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = false

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = false
}
