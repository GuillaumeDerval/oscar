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
 * Abstract class for linear expressions: c_0 + sum_i (c_i * v_i)
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Aurélien Crucifix acr@n-side.com
 */
abstract class LinearExpression extends Expression {

  /**
   * The constant coefficient for this [[LinearExpression]]: c_0
   */
  val constant: Double

  /**
   * A [[Map]] of the [[Var]] involved in this [[LinearExpression]] with their coefficient (non-zero): v_i -> c_i
   */
  val coefficients: Map[Var, Double]


  def +(expr: LinearExpression): LinearExpression = LinearExpressionSum(expr, this)
  def -(expr: LinearExpression): LinearExpression = LinearExpressionDiff(this, expr)
  def unary_- = LinearExpressionDiff(zero, this)

  def *(c: Const): LinearExpression = LinearExpressionProd(c, this)
  def /(c: Const): LinearExpression = LinearExpressionFrac(this, c)

  def <:=(linExpr: LinearExpression) = LinearConstraintExpression(this - linExpr, LQ)
  def >:=(linExpr: LinearExpression) = LinearConstraintExpression(this - linExpr, GQ)
  def =:=(linExpr: LinearExpression) = LinearConstraintExpression(this - linExpr, EQ)


  override def uses[V <: Var](v: V) = coefficients.contains(v)

  override def eval(env: Var => Double): Double = constant + coefficients.map(e => env(e._1) * e._2).sum

  override def value: Option[Double] =
    Some(
      constant + coefficients.flatMap { case (x, a) =>
        x.value.map(v => a * v)
      }.sum
    )

  override def derive(x: Var): Expression = coefficients.get(x) match {
    case None    => zero
    case Some(v) => Const(v)
  }

  def canEqual(that: Any) = that.isInstanceOf[LinearExpression]

  override def equals(that: Any) = that match {
    case other: LinearExpression =>
      (this canEqual that) &&
        (other.constant equals this.constant) &&
        (other.coefficients equals this.coefficients)
    case _ => false
  }

  override def hashCode: Int = constant.hashCode * 13 + MurmurHash3.mapHash(coefficients)
}

/**
 * Computes the constant and the coefficients of the [[LinearExpression]]
 * resulting from a linear combination of two [[LinearExpression]]
 */
trait LinearCombination { self: LinearExpression with BinaryOp[LinearExpression, LinearExpression] =>

  val constant = operation(lhs.constant, rhs.constant)

  val coefficients = {
    val mymap = scala.collection.mutable.Map[Var, Double]()

    for ((vari, coefLhs) <- lhs.coefficients) {
      mymap += vari -> coefLhs
    }

    for ((vari, coefRhs) <- rhs.coefficients) {
      mymap.get(vari) match {
        case Some(coefLhs) => mymap(vari) = operation(coefLhs, coefRhs)
        case None => mymap += (vari -> operation(0.0, coefRhs))
      }
    }

    // Do not store variables with null coefficient
    mymap.filterNot(_._2 == 0).toMap
  }
}

case class LinearExpressionSum(lhs: LinearExpression, rhs: LinearExpression)
  extends LinearExpression with SumOp[LinearExpression, LinearExpression] with LinearCombination

case class LinearExpressionDiff(lhs: LinearExpression, rhs: LinearExpression)
  extends LinearExpression with DiffOp[LinearExpression, LinearExpression] with LinearCombination

case class LinearExpressionProd(lhs: Const, rhs: LinearExpression)
  extends LinearExpression with ProdOp[Const, LinearExpression] {

  val constant = lhs.constant * rhs.constant
  val coefficients = rhs.coefficients.mapValues(coef => coef * lhs.constant).view.force.filter(_._2 == 0.0)
}

case class LinearExpressionFrac(lhs: LinearExpression, rhs: Const)
  extends LinearExpression with FracOp[LinearExpression, Const] {

  val constant = lhs.constant / rhs.constant
  val coefficients = lhs.coefficients.mapValues(coef => coef / rhs.constant).view.force.filter(_._2 == 0.0)
}

/**
 * Represents the product between a [[Const]] and a [[Var]]: coefficient * variable
 */
case class ConstVar(coefficient: Const, variable: Var) extends LinearExpression with ProdOp[Const, Var] {

  val lhs = coefficient
  val rhs = variable

  val constant = 0.0
  val coefficients =
    if (coefficient == zero) Map[Var, Double]()
    else Map(variable -> coefficient.constant)
}
