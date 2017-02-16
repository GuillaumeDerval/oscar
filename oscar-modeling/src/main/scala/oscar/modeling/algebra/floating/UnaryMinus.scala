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
 * Expression of -value
  *
  * @param value the value of the constant
 */
case class UnaryMinus(value: FloatExpression) extends FloatExpression {
  override def evaluate(): Double = -value.evaluate()
  override def min: Double = -value.max
  override def max: Double = -value.min

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = Array(value)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = UnaryMinus(func(value).asInstanceOf[FloatExpression])

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = value.continuous

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = value.linear

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}
