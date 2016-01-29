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
 * (c * linExpr)
 */
class LinearExpressionProd(val c: Const, val expr: LinearExpression) extends LinearExpression {
  import scala.collection.immutable.Map
  val cte = if (c == Zero) 0.0 else c.d * expr.cte
  val coef = if (c == Zero) Map[Var, Double]() else expr.coef.map(e => (e._1 -> c.d * e._2))

  override def derive(v: Var): Expression = {
    c * expr.derive(v)
  }
  override def toString = c + "*(" + expr + ")"

} 
