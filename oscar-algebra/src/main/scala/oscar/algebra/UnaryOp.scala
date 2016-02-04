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
 * Represents unary operations on an [[Expression]]
 */
trait UnaryOp[O <: Expression] { self: Expression =>

  /**
   * The operand of the unary operation
   */
  val operand: O

  /**
   * The symbol representing the operation (i.e. 'cos' for the cosine)
   */
  val symbol: String

  /**
   * Returns the result of applying the unary operation on the value of the operand
   */
  protected def operation(operandValue: Double): Double

  override def uses[V <: Var](v: V) = operand.uses(v)

  override def value = operand.value.map(v => operation(v))

  override def eval(env: Var => Double) = operation(operand.eval(env))

  override def isZero = operand.isZero

  override def toString = s"$symbol( $operand )" // TODO improve me !
}
