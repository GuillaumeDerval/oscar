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

import oscar.algebra.linear.Var

/**
 * Abstract class for algebraic expressions
 */
abstract class Expression {

  /**
   * Returns true if this [[Expression]] uses the given [[Var]]
   */
  def uses[V <: Var](v: V): Boolean

  /**
   * Returns the value of this [[Expression]] if it has any
   */
  def value: Option[Double]

  /**
   * Evaluates this [[Expression]] for the given assignment of the [[Var]]
   */
  def eval(env: Var => Double): Double

  def +(expr: Expression): Expression = new Sum(this, expr)

  def -(expr: Expression): Expression = new Diff(this, expr)

  def *(expr: Expression): Expression = new Prod(this, expr)

  def /(expr: Expression): Expression = new Frac(this, expr)

  /**
   * Derives this [[Expression]] with respect to the given [[Var]]
   */
  def derive(v: Var): Expression

  def isZero: Boolean = false
}
  
