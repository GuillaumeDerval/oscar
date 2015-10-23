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

import oscar.linprog.enums.ModelExportFormat
import oscar.linprog.interface.gurobi.GurobiLib
import oscar.linprog.interface.lpsolve.LPSolveLib

/**
 * Describes the solver library with its particularities.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class MPSolverLib[+I <: MPSolverInterface](val name: String) {
  def createSolver: I

  /**
   * Returns the list of [[ModelExportFormat]] supported by this solver library
   */
  def supportedModelExportFormats: Seq[ModelExportFormat]
}

object MPSolverLib {
  def canInstantiate[I <: MPSolverInterface](lib: MPSolverLib[I]): Boolean =
    try {
      val s = lib.createSolver
      s.release()
      true
    } catch {
      case e: UnsatisfiedLinkError => println(e.getMessage); false
      case e: NoClassDefFoundError => println(e.getMessage); false
    }

  def solvers: List[MPSolverLib[MPSolverInterface]] = List(LPSolveLib, GurobiLib).filter(canInstantiate)
  def lpSolvers: List[MPSolverLib[MPSolverInterface]] = List(LPSolveLib, GurobiLib).filter(canInstantiate)
  def mipSolvers: List[MPSolverLib[MIPSolverInterface]] = List(LPSolveLib, GurobiLib).filter(canInstantiate)
}
