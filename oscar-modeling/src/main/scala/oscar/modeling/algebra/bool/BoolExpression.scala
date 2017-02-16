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

package oscar.modeling.algebra.bool

import oscar.modeling.algebra.Expression
import oscar.modeling.algebra.integer._
import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.misc.{EmptyDomainException, VariableNotBoundException}
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.BoolVar

/**
 * Represents a Boolean expression (an IntExpression that returns a boolean, 0 or 1)
 */
trait BoolExpression extends IntExpression {
  /**
   * Return a lower bound for this expression
   */
  def min: Int = 0

  /**
   * Return a higher bound for this expression
   */
  def max: Int = 1

  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int = if(evaluateBool()) 1 else 0
  def evaluateBool(): Boolean

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  override def reify()(implicit modelDeclaration: ModelDeclaration): BoolVar = {
    val z = BoolVar(min == 0, max == 1)(modelDeclaration)
    modelDeclaration.post(this === z)
    z
  }

  /**
   * Get an iterator to all the values that this expression can take
   */
  override def values(): Iterable[Int] = Set(0, 1)

  def toConstraint: Constraint = ExpressionConstraint(this)
  def ^(b: BoolExpression): BoolExpression = Xor(this, b)
  def &(b: BoolExpression): BoolExpression = {
    (this, b) match {
      case (And(x), And(y)) => And(x ++ y)
      case (And(x), second) => And(x ++ Array(second))
      case (first , And(y)) => And(y ++ Array(first))
      case (first , second) => And(first, second)
    }
  }
  def |(b: BoolExpression): BoolExpression = {
    (this, b) match {
      case (Or(x), Or(y) ) => Or(x ++ y)
      case (Or(x), second) => Or(x ++ Array(second))
      case (first, Or(y) ) => Or(y ++ Array(first))
      case (first, second) => Or(first, second)
    }
  }
  def ==> (b: BoolExpression): BoolExpression = Implication(this, b)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (Expression => Expression)): BoolExpression
}

object BoolExpression {
  /**
   * Convert a BoolExpression to an equivalent constraint
   */
  implicit def booltoConstraint(boolExpression: BoolExpression): Constraint = boolExpression.toConstraint
  implicit def toBoolExpression(intExpression: IntExpression): BoolExpression = intExpression !== 0
  implicit def intToConstraint(intExpression: IntExpression): Constraint = intExpression !== 0
}