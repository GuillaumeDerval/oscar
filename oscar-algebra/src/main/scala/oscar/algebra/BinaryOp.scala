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

abstract class BinaryOp[+T <: AnyType] extends Expression[T] {
  val a: Expression[T]
  val b: Expression[T]
  val symb: String
  val op: (Double, Double) => Double
  override def toString = "(" + a + symb + b + ")"

  override def eval(env: Var => Double) = {
    op(a.eval(env), b.eval(env))

  }

  override def value = {
    (a.value, b.value) match {
      case (Some(va), Some(vb)) => Some(op(va, vb))
      case _ => None
    }
  }
}
