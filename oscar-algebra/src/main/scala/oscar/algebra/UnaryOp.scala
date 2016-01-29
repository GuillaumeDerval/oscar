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
 * Abstract class used to represent a unary operation on an [[Expression]]
 */
abstract class UnaryOp(expr: Expression, name: String, f: Double => Double) extends Expression {

  override def value = expr.value.map(v => f(v))

  override def eval(env: Var => Double) = f(expr.eval(env))

  override def toString = name + "(" + expr + ")"
}
