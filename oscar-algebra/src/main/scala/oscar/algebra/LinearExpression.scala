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
package oscar.algebra

/**Abstract type for variables*/

/**
 * @author Pierre Schaus pschaus@gmail.com 
 */
abstract class LinearExpression extends Expression {

  val cte: Double
  val coef: scala.collection.immutable.Map[Var, Double]

  def uses[V <: Var](v: V) = coef.contains(v)

  def +(expr: LinearExpression): LinearExpression = new LinearExpressionSum(expr, this)

  def -(expr: LinearExpression): LinearExpression = new LinearExpressionDiff(this, expr)

  def unary_- : LinearExpression = new LinearExpressionDiff(0, this)

  def <:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, LQ)

  def >:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, GQ)

  def =:=(linExpr: LinearExpression) = new LinearConstraintExpression(this - linExpr, EQ)

  /**
   * Test if two linear expressions are logically equivalent
   */
  override def equals(that: Any) = {
    that match {
      case other: LinearExpression => {
        other.cte == cte && other.coef == coef
      }
      case _ => false
    }
  }

  def eval(env: Var => Double): Double = cte + coef.map(e => env(e._1) * e._2).sum

  override def value: Option[Double] = {
    var res = cte
    for ((x, a) <- coef) {
      x.value match {
        case None => return None
        case Some(v) => {
          res += a * v
        }
      }
    }
    Some(res)
  }

  override def derive(x: Var): Expression = {
    coef.get(x) match {
      case None => Zero
      case Some(v: Double) => new Const(v)
    }
  }
}

