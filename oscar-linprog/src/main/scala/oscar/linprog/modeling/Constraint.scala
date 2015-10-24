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

package oscar.linprog.modeling

import oscar.algebra._
import oscar.linprog.interface.{MIPSolverInterface, InfeasibilityAnalysisInterface, MPSolverInterface}

/**
 * Represents a constraint in a mathematical programming model.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class AbstractMPConstraint[+I <: MPSolverInterface](val name: String)(implicit solver: MPSolver[I]) {
  def addToSolver(): Unit

  addToSolver()

  override def toString = name
}

class LinearConstraint[+I <: MPSolverInterface] protected (
  name: String,
  val expression: oscar.algebra.LinearConstraintExpression
  )(implicit solver: MPSolver[I]) extends AbstractMPConstraint[I](name) {

  def addToSolver(): Unit = solver.addLinearConstraint(this)

  /**
   * Returns true in case this Constraint belongs to the set of infeasible constraints
   */
  def infeasible(implicit ev: I => InfeasibilityAnalysisInterface): Option[Boolean] = solver.isLinearConstraintInfeasible(name).toOption

  override def toString: String = name + ": " + expression
}

object LinearConstraint {
  def apply[I <: MPSolverInterface](name: String, expression: oscar.algebra.LinearConstraintExpression)(implicit solver: MPSolver[I]) =
    new LinearConstraint[I](name, expression)
}

class IndicatorConstraint[+I <: MPSolverInterface] private (
  name: String,
  override val expression : oscar.algebra.IndicatorConstraintExpression
  )(implicit solver: MPSolver[I]) extends LinearConstraint[I](name, expression) {

  if(expression.indicators.length > 0)
    require(expression.bigM.isDefined, s"M should be defined for the IndicatorConstraint $name")

  override def addToSolver(): Unit = solver.addIndicatorConstraint(this)
}

object IndicatorConstraint {
  def apply[I <: MPSolverInterface](name: String, expression : oscar.algebra.IndicatorConstraintExpression)(implicit solver: MPSolver[I]) =
    new IndicatorConstraint[I](name, expression)
}
