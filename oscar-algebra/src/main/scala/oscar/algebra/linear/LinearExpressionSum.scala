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
package oscar.algebra.linear

import oscar.algebra.Expression

/**
 * (linExpr1 + linExpr2)
 */
class LinearExpressionSum(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1, expr2) {

  val opStr = "+"

  def op(v1: Double, v2: Double) = v1 + v2

  override def derive(v: Var): Expression = {
    expr1.derive(v) + expr2.derive(v)
  }
}
