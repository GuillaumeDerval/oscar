/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression

/**
 * Sum of an array of expression
 */
case class Sum(v: Array[FloatExpression]) extends FloatExpression {
  override def evaluate(): Double = v.foldLeft(0.0)((acc: Double, e: FloatExpression) => acc + e.evaluate())
  override def min: Double = v.foldLeft(0.0)((acc: Double, e: FloatExpression) => acc + e.min)
  override def max: Double = v.foldLeft(0.0)((acc: Double, e: FloatExpression) => acc + e.max)

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = v

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = new Sum(v.map(func).asInstanceOf[Array[FloatExpression]])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = subexpressions().forall(_.continuous)

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = subexpressions().forall(_.linear)

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}

object Sum {
  def apply(a: FloatExpression*): Sum = Sum(a.toArray)

  def apply(v: Iterable[FloatExpression]): Sum = Sum(v.toArray)

  def apply[A](indices: Iterable[A])(f: A => FloatExpression): Sum = Sum(indices map f)

  def apply[A, B](indices1: Iterable[A], indices2: Iterable[B])(f: (A, B) => FloatExpression): Sum = Sum(for (i <- indices1; j <- indices2) yield f(i, j))

  def apply(n1: Int, n2: Int)(f: (Int, Int) => FloatExpression): Sum = Sum(0 until n1, 0 until n2)(f)
}