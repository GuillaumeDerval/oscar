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

import oscar.modeling.algebra.bool.{And, BoolExpression, Or}

/**
 * Basic operations on expressions
 */
trait IntExpressionOperators {
  def abs(a: IntExpression): IntExpression = Abs(a)
  def min(a: IntExpression*): IntExpression = Min(a.toArray)
  def min(a: Array[IntExpression]): IntExpression = Min(a)
  def max(a: IntExpression*): IntExpression = Max(a.toArray)
  def max(a: Array[IntExpression]): IntExpression = Max(a)
  def sum(a: IntExpression*): IntExpression = Sum(a.toArray)
  def sum(a: Array[IntExpression]): IntExpression = Sum(a)
  def count(X: Array[IntExpression], Y: IntExpression) = Count(X, Y)
}
