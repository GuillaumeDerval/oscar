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

import oscar.algebra.linear.Var

/**
 * (expr1 + expr2) or (expr1 - expr2)
 */
abstract class LinearExpressionBinary(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpression {

  val cte = op(expr1.cte, expr2.cte)
  val coef = merge()

  def op(v1: Double, v2: Double): Double

  val opStr: String

  override def toString() = "(" + expr1 + opStr + expr2 + ")"

  def merge(): scala.collection.immutable.Map[Var, Double] = {
    import scala.collection.mutable.Map
    val mymap = Map[Var, Double]()
    for ((k, v) <- expr1.coef) {
      mymap += k -> v
    }
    for ((k, v) <- expr2.coef) {
      mymap.get(k) match {
        case Some(c) => mymap(k) = op(c, v)
        case None => mymap += (k -> op(0, v))
      }
    }
    mymap.filterNot(_._2 == 0).toMap
  }

}  
