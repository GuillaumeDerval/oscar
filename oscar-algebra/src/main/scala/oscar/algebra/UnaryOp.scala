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

//Unary operators
abstract class UnaryOp(expr: Expression[AnyType], name: String, f: Double => Double) extends AbstractExpression[AnyType] {

  override def toString = name + "(" + expr + ")"

  def value = expr.value match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def eval(env: Var => Double) = f(expr.eval(env))
}
