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

import oscar.algebra.linear.{Const, Var}

/**
 * Represents a logarithm base
 */
sealed class LogBase(val b: Double) {
  require(b > 0.0, "The logarithm base should be a positive real.")
  require(b != 1.0, "The logarithm base should not be 1.")
}

object LogBase {
  def apply(b: Double): LogBase = new LogBase(b)
  def unapply(logBase: LogBase): Option[Double] = Some(logBase.b)
}

/**
 * The natural logarithm base: e
 * "e" being Euler's constant, an irrational number approximately equal to 2.718281828459
 */
case object NaturalBase extends LogBase(2.718281828459)

case object Base10 extends LogBase(10)

/**
 * Represents the logarithm in the given base: log_b(x)
 */
case class Log(operand: Expression, base: LogBase = NaturalBase) extends Expression with UnaryOp[Expression] {

  val symbol = "log"

  override protected def operation(operandValue: Double): Double = base match {
    case NaturalBase => math.log(operandValue)
    case Base10 => math.log10(operandValue)
    case LogBase(b) => math.log(operandValue) / math.log(b)
  }

  override def derive(v: Var): Expression = {
    val dln = operand.derive(v) / operand

    base match {
      case NaturalBase => dln
      case LogBase(b) => dln / math.log(b)
    }
  }

  override def isZero = operand match {
    case Const(1) => true
    case _ => false
  }
}
