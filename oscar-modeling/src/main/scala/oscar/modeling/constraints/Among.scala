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

package oscar.modeling.constraints

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

/**
  * N variables of X take a values in set S
  */
case class Among(N: IntExpression, X: Array[IntExpression], S: Set[Int]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = Array(N) ++ X

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object Among
{
  /**
    * N variables of X take value v
    */
  def apply(N: IntExpression, X: Array[IntExpression], v: Int): Among = Among(N, X, Set(v))
}

object AtLeast
{
  /**
    * AtLeast Constraint: at least n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } >= n
    */
  def apply(n: Int, x: Array[IntExpression], s: Set[Int])(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(n, x.size), x, s)
  }

  /**
    * AtLeast Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } >= n
    */
  def apply(n: Int, x: Array[IntExpression], v: Int)(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(n, x.size), x, Set(v))
  }
}

object AtMost
{
  /**
    * AtMost Constraint: at most n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } <= n
    */
  def apply(n: Int, x: Array[IntExpression], s: Set[Int])(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(0, n), x, s)
  }

  /**
    * AtMost Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } <= n
    */
  def apply(n: Int, x: Array[IntExpression], v: Int)(implicit modelDeclaration: ModelDeclaration)= {
    Among(IntVar(0, n), x, Set(v))
  }
}