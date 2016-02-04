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

import oscar.algebra._

/**
 * Represents a constant with the given value d
 */
case class Const(constant: Double) extends LinearExpression {

  val coefficients: Map[Var, Double] = Map()


  override def *(c2: Const) = Const(constant * c2.constant)
  def +(c2: Const) = Const(constant + c2.constant)
  def -(c2: Const) = Const(constant - c2.constant)

  def *(x: Var) = ConstVar(this, x)

  def *(expr: LinearExpression) = LinearExpressionProd(this, expr)


  override def uses[V <: Var](v: V) = false

  override def eval(env: Var => Double): Double = constant

  override def value: Option[Double] = Some(constant)

  override def derive(v: Var): Expression = Const(0.0)

  override def toString = constant.toString
}
