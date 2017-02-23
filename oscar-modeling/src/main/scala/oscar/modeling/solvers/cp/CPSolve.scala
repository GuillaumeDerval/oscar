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

package oscar.modeling.solvers.cp

import oscar.algo.search.Branching
import Branchings.{Alternative, BranchingInstantiator}
import oscar.modeling.solvers.Solve


/**
  * Contains the needed data for a simple CPSolver: branching and solution management
  *
  * @tparam RetVal
  */
trait CPSolve[RetVal] extends Solve[RetVal] {
  protected var branching: BranchingInstantiator = null
  protected var on_solution: () => RetVal = null

  def getSearch: BranchingInstantiator = branching
  def setSearch(b: BranchingInstantiator): Unit = branching = b
  def setSearch(b: Branching): Unit = branching = (_) => b
  def setSearch(b: => Seq[Alternative]): Unit = branching = Branchings(b)

  def onSolution = on_solution
  def onSolutionF(o: () => RetVal): Unit = on_solution = o
}
