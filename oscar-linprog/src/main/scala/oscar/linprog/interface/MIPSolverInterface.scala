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
 * to MIPs (such as the addition of integer variables).
 *
 * @author acrucifix acr@n-side.com
 */
trait MIPSolverInterface extends MPSolverInterface {
  /**
   * Adds a new integer variable (column) to the model.
   *
   * @param name the name of the variable in the model
   * @param from the lower bound (default is -Inf)
   * @param to the upper bound (default is +Inf)
   * @param objCoef the coefficient in the objective function
   * @param cstrCoefs the non-zero constraint coefficients
   * @param cstrIds the indices of the corresponding linear constraints
   *
   * @return the index of the variable (column number) in the model
   */
  def addIntegerVariable(name: String, from: Int, to: Int,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None): Int

  /**
   * Adds a new binary (0-1) integer variable (column) to the model.
   *
   * @param name the name of the variable in the model
   * @param objCoef the coefficient in the objective function
   * @param cstrCoefs the non-zero constraint coefficients
   * @param cstrIds the indices of the corresponding linear constraints
   *
   * @return the index of the variable (column number) in the model
   */
  def addBinaryVariable(name: String,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None): Int

  /**
   * Returns true if the specified variable is an integer variable.
   */
  def isInteger(varId: Int): Boolean

  /**
   * Sets the specified variable as an integer variable.
   */
  def setInteger(varId: Int)

  /**
   * Returns true if the specified variable is a binary 0-1 integer variable.
   */
  def isBinary(varId: Int): Boolean

  /**
   * Set the specified variable as a binary 0-1 integer variable.
   */
  def setBinary(varId: Int)

  /**
   * Returns true if the specified variable is a float variable.
   */
  def isFloat(varId: Int): Boolean

  /**
   * Set the specified variable as a float variable.
   */
  def setFloat(varId: Int)
}
