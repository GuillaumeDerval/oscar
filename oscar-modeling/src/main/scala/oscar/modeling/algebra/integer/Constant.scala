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

package oscar.modeling.algebra.integer

import oscar.modeling.algebra.Expression

/**
 * Expression of a constant
  *
  * @param value the value of the constant
 */
case class Constant(value: Int) extends IntExpression {
  override def evaluate(): Int = value
  override def min: Int = value
  override def max: Int = value
  override def values(): Iterable[Int] = List(value)

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = Array[IntExpression]()

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): IntExpression = this

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = true
}