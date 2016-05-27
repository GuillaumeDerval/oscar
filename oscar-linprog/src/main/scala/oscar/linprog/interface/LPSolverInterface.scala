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
 * to LPs (such as information about the dual).
 *
 * @author acrucifix acr@n-side.com
 */
trait LPSolverInterface extends MPSolverInterface {
  /**
   * Returns the reduced costs of the variables in the solution.
   *
   * Note: it is the vector corresponding to the values in the solution of the duals of the variable bounds.
   */
  def reducedCosts: Vector[Double]

  /**
   * Get the reduced cost corresponding to the given variable in the solution.
   *
   * Note: the reduced cost is the value of the dual of the variable bounds.
   */
  def getReducedCost(varId: Int): Double

  /**
   * Returns the vector of the values in the solution of the dual variables corresponding to the linear constraint.
   */
  def constraintDualValues: Double

  /**
   * Return the value in the solution of the dual to the given constraint.
   */
  def getConstraintDualValue(cstrId: Int): Double
}
