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

package oscar.modeling.solvers.cp.distributed

import oscar.modeling.solvers.cp.CPSolve
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy


/**
  * Allow to decompose into subproblems for solving in a distributed environment
  *
  * @tparam RetVal
  */
trait DecomposedCPSolve[RetVal] extends CPSolve[RetVal] {
  private var decomposition_strategy: DecompositionStrategy = null
  def setDecompositionStrategy(d: DecompositionStrategy): Unit = decomposition_strategy = d
  def getDecompositionStrategy: DecompositionStrategy = decomposition_strategy
}