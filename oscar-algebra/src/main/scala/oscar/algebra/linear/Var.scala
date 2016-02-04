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
package oscar.algebra.linear

import oscar.algebra._

/**
 * Represents a named variable: v_i
 */
class Var(val name: String) extends LinearExpression {

  val constant = 0.0
  val coefficients: Map[Var, Double] = Map(this -> 1.0)


  override def *(c: Const): ConstVar = ConstVar(c, this)


  override def derive(v: Var): Expression =
    if (v equals this) one
    else zero

  override def equals(that: Any) = that match {
    case other: Var => other.name equals this.name
    case _ => false
  }

  override def hashCode: Int = name.hashCode

  override def toString = name
}

object Var {
  def apply(name: String) = new Var(name)
}
