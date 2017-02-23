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

/**
 * Basic operations on expressions
 */
trait FloatExpressionOperators {
  def abs(a: FloatExpression): FloatExpression = Abs(a)
  def min(a: FloatExpression*): FloatExpression = Min(a.toArray)
  def min(a: Array[FloatExpression]): FloatExpression = Min(a)
  def max(a: FloatExpression*): FloatExpression = Max(a.toArray)
  def max(a: Array[FloatExpression]): FloatExpression = Max(a)
  def sum(a: FloatExpression*): FloatExpression = Sum(a.toArray)
  def sum(a: Array[FloatExpression]): FloatExpression = Sum(a)
}