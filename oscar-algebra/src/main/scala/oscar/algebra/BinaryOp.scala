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
 * Abstract class used to represent a binary operation on two [[Expression]]
 */
abstract class BinaryOp(
  val left: Expression,
  val right: Expression,
  val symbol: String,
  operation: (Double, Double) => Double) extends Expression {

  override def uses[V <: Var](v: V) = left.uses(v) || right.uses(v)

  override def eval(env: Var => Double) = operation(left.eval(env), right.eval(env))

  override def value = (left.value, right.value) match {
    case (Some(leftValue), Some(rightValue)) => Some(operation(leftValue, rightValue))
    case _ => None
  }

  override def isZero = left.isZero && right.isZero

  override def toString = s"$left $symbol $right"
}
