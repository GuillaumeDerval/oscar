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
 * Represents binary operations between two [[Expression]]
 */
trait BinaryOp[+L <: Expression, +R <: Expression] { self: Expression =>

  /**
   * The left hand side of the binary operation
   */
  val lhs: L

  /**
   * The right hand side of the binary operation
   */
  val rhs: R

  /**
   * The symbol representing the operation (i.e. '+' for the sum)
   */
  val symbol: String

  /**
   * Returns the result of applying the binary operation on the values of the left hand side and the right hand side.
   */
  protected def operation(lhsValue: Double, rhsValue: Double): Double

  override def uses[V <: Var](v: V) = lhs.uses(v) || rhs.uses(v)

  override def eval(env: Var => Double) = operation(lhs.eval(env), rhs.eval(env))

  override def value: Option[Double] = (lhs.value, rhs.value) match {
    case (Some(leftValue), Some(rightValue)) => Some(operation(leftValue, rightValue))
    case _ => None
  }

  override def isZero = lhs.isZero && rhs.isZero

  override def toString = s"$lhs $symbol $rhs"
}

/**
 * Represents the sum of two [[Expression]]: lhs + rhs
 */
trait SumOp[+L <: Expression, +R <: Expression] extends BinaryOp[L, R] { self: Expression =>

  val symbol = "+"

  override protected def operation(lhsValue: Double, rhsValue: Double): Double = lhsValue + rhsValue

  override def derive(v: Var): Expression = lhs.derive(v) + rhs.derive(v)
}

/**
 * Represents the difference of two [[Expression]]: lhs - rhs
 */
trait DiffOp[+L <: Expression, +R <: Expression] extends BinaryOp[L, R] { self: Expression =>

  val symbol = "-"

  override protected def operation(lhsValue: Double, rhsValue: Double): Double = lhsValue - rhsValue

  override def derive(v: Var): Expression = lhs.derive(v) - rhs.derive(v)
}

/**
 * Represents the product of two [[Expression]]: lhs * rhs
 */
trait ProdOp[L <: Expression, R <: Expression] extends BinaryOp[L, R] { self: Expression =>

  val symbol = "*"

  override protected def operation(lhsValue: Double, rhsValue: Double): Double = lhsValue * rhsValue

  override def derive(v: Var): Expression = lhs * rhs.derive(v) + rhs * lhs.derive(v)

  override def isZero = lhs.isZero || rhs.isZero

  override def toString = s"($lhs) $symbol ($rhs)" // TODO improve me
}

/**
 * Represents the division of two [[Expression]]: lhs / rhs
 */
trait FracOp[+L <: Expression, +R <: Expression] extends BinaryOp[L, R] { self: Expression =>

  val numerator = lhs
  val denominator = rhs

  val symbol = "/"

  override protected def operation(lhsValue: Double, rhsValue: Double): Double = lhsValue / rhsValue

  override def derive(v: Var): Expression = {
    val fprime = numerator.derive(v)
    val gprime = denominator.derive(v)

    (fprime * denominator - gprime * numerator) / (denominator * denominator)
  }

  override def isZero = numerator.isZero

  override def toString = s"($lhs) $symbol ($rhs)" // TODO improve me
}
