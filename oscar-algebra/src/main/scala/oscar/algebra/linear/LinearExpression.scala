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

import scala.util.hashing.MurmurHash3

/**
 * Abstract class for linear expressions
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Aurélien Crucifix acr@n-side.com
 */
abstract class LinearExpression(constant: Double, coefficients: Map[Var, Double]) extends Expression {

  override def uses[V <: Var](v: V) = coefficients.contains(v)

  def +(expr: LinearExpression): LinearExpression = new LinearExpressionSum(expr, this)

  def -(expr: LinearExpression): LinearExpression = new LinearExpressionDiff(this, expr)

  def unary_- : LinearExpression = new LinearExpressionDiff(Zero, this)

  def <:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, LQ)

  def >:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, GQ)

  def =:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, EQ)

  override def eval(env: Var => Double): Double = constant + coefficients.map(e => env(e._1) * e._2).sum

  override def value: Option[Double] =
    Some(
      constant + coefficients.flatMap { case (x, a) =>
        x.value.map(v => a * v)
      }.sum
    )

  override def derive(x: Var): Expression = {
    coefficients.get(x) match {
      case None => Zero
      case Some(v: Double) => new Const(v)
    }
  }

  override def equals(that: Any) = {
    that match {
      case other: LinearExpression => {
        other.constant == this.constant && other.coefficients == this.coefficients
      }
      case _ => false
    }
  }

  override def hashCode: Int = constant.hashCode * 13 + MurmurHash3.mapHash(coefficients)
}

