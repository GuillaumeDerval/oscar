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

abstract class Expression {

  def value: Option[Double]

  def eval(env: Var => Double): Double

  def +(expr: Expression): Expression = Sum(this, expr)

  def -(expr: Expression): Expression = new Diff(this, expr)

  def *(expr: Expression): Expression = new Prod(this, expr)

  def /(expr: Expression): Expression = new Frac(this, expr)

  def <= (expression: Expression) = new Equation(this-expression, LQ)
  def >= (expression: Expression) = new Equation(this-expression, GQ)
  def === (expression: Expression) = new Equation(this-expression, EQ)

  def derive(x: Var): Expression

  def isZero(): Boolean = false
}
  
