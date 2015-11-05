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

package oscar.linprog.interface

/**
 * This interface complements the [[MPSolverInterface]] by adding functions specific
 * to infeasibility analysis.
 *
 * @author acrucifix acr@n-side.com
 */
trait InfeasibilityAnalysisInterface { self: MPSolverInterface =>
  /**
   * Finds the sources of infeasibilities in the problem.
   *
   * @return true if the infeasibility analysis could be performed
   */
  def analyseInfeasibility(): Boolean

  /**
   * Returns true if the lower bound of the given variable
   * belongs to the set of constraints making the problem infeasible
   */
  def isVariableLowerBoundInfeasible(varId: Int): Boolean

  /**
   * Returns true if the upper bound of the given variable
   * belongs to the set of constraints making the problem infeasible
   */
  def isVariableUpperBoundInfeasible(varId: Int): Boolean

  /**
   * Returns true if the given constraint
   * belongs to the set of constraints making the problem infeasible
   */
  def isLinearConstraintInfeasible(cstrId: Int): Boolean
}
