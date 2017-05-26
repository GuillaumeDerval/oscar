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

/**
  * Lexicographical Less or Equal constraint.
  *
  * Given tuples (a1, a2, ...) and (b1, b2, ...),
  * this ensures
  *
  * a1 <= b1 || (a1 == b1 && LexLeq((a2, ...), (b2, ...)))
 *
  * @param a
  * @param b
  */
case class LexLeq(a: Array[IntExpression], b: Array[IntExpression]) extends Constraint {
  /**
   * @return a list of all the IntExpression associated to this constraint
   */
  override def getIntExpressions(): Iterable[IntExpression] = a ++ b

  /**
   * @return a list of all the FloatExpression associated to this constraint
   */
  override def getFloatExpressions(): Iterable[FloatExpression] = Array[FloatExpression]()
}

object LexGeq
{
  /**
    * Lexicographical "Greater or Equal than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 <= b1 || (a1 == b1 && LexGeq((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = LexLeq(b,a)
}

object LexLr
{
  /**
    * Lexicographical "Strictly Lesser than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 < b1 || (a1 == b1 && LexLr((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = {
    a(a.length-1) += 1
    LexLeq(a,b)
  }
}

object LexGr
{
  /**
    * Lexicographical "Strictly Greater than" constraint.
    *
    * Given tuples (a1, a2, ...) and (b1, b2, ...),
    * this ensures
    *
    * a1 > b1 || (a1 == b1 && LexGr((a2, ...), (b2, ...)))
    * @param a
    * @param b
    */
  def apply(a: Array[IntExpression], b: Array[IntExpression]) = {
    b(b.length-1) += 1
    LexLeq(b,a)
  }
}