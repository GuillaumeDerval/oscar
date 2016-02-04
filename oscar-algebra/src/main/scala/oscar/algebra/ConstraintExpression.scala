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

import oscar.algebra.linear.LinearExpression

/**
 * Represents the sense of a [[ConstraintExpression]]: <=, == or >=
 */
sealed class ConstraintSense(val symbol: String, val name: String) {
  lazy val opposite = ConstraintSense.opposite(this)

  override def toString = symbol
}

case object LQ extends ConstraintSense("<=", "LQ")
case object EQ extends ConstraintSense("==", "EQ")
case object GQ extends ConstraintSense(">=", "GQ")

object ConstraintSense {
  /**
   * Lists all the possible [[ConstraintSense]]
   */
  val values: List[ConstraintSense] = List(LQ, EQ, GQ)

  /**
   * Returns the [[ConstraintSense]] opposite to the one given.
   */
  def opposite(sense: ConstraintSense): ConstraintSense = sense match {
    case LQ => GQ
    case EQ => EQ
    case GQ => LQ
  }
}

/**
 * Represents the expression of a constraint: expression sense 0.0
 */
class ConstraintExpression[E <: Expression](val expression: E, val sense: ConstraintSense) {

  override def equals(that: Any) = that match {
    case other: ConstraintExpression[_] =>
      ((other.expression equals this.expression) && (other.sense equals this.sense)) ||
        ((other.expression equals (oscar.algebra.zero-this.expression)) && (other.sense equals this.sense.opposite))
    case _ => false
  }

  override def hashCode: Int = ???

  override def toString = s"$expression ${sense.symbol} 0"
}

object ConstraintExpression {
  def apply[E <: Expression](expression: E, sense: ConstraintSense) = new ConstraintExpression[E](expression, sense)
}

/**
 * Represents a [[ConstraintExpression]] with a [[LinearExpression]]
 */
class LinearConstraintExpression(linearExpression: LinearExpression, sense: ConstraintSense)
  extends ConstraintExpression[LinearExpression](linearExpression, sense)

object LinearConstraintExpression {
  def apply(expression: LinearExpression, sense: ConstraintSense) = new LinearConstraintExpression(expression, sense)
}