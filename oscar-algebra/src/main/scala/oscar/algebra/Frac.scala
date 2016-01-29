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
 * Represents the division of two [[Expression]]: numerator / denumerator
 */
class Frac(val numerator: Expression, val denominator: Expression)
  extends BinaryOp(numerator, denominator, "/", (a: Double, b: Double) => a / b) {

  override def derive(v: Var): Expression = {
    val fprime = numerator.derive(v)
    val gprime = denominator.derive(v)

    (fprime * denominator - gprime * numerator) / (denominator * denominator)
  }

  override def isZero = numerator.isZero

  override def toString = s"($left) $symbol ($right)" // TODO improve me
}
