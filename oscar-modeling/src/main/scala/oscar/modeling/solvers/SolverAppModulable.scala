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

package oscar.modeling.solvers

import oscar.modeling.models.ModelDeclaration

/**
  * Trait that adds a "module" (a type of solver) to a SolverApp
  */
trait SolverAppModulable {
  val md: ModelDeclaration
  val app: SolverApp[_]

  /**
    * Returns a list of module. For all modules, should be in the form
    * myModule :: super.getModules()
    */
  def getModules: List[SolverAppModule] = Nil
}
