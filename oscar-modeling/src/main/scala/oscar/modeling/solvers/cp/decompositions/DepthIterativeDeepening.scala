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

package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings._
import oscar.modeling.solvers.cp.distributed.SubProblem
import oscar.modeling.vars.IntVar

import scala.util.Random

/**
  * Iterative deepening based on depth
  * @param searchInstantiator the search to use
  */
class DepthIterativeDeepening(searchInstantiator: BranchingInstantiator) extends IterativeDeepeningStrategy[Int](searchInstantiator) {
  override def initThreshold(model: MemoCPModel, subProblemsNeeded: Int): Int = 1

  override def nextThreshold(oldThreshold: Int, currentSubproblems: List[SubProblem], subProblemsNeeded: Int): Int = oldThreshold+1

  override def shouldStopSearch(model: MemoCPModel, threshold: Int, depth: Int, discrepancy: Int): Boolean = depth >= threshold
}

