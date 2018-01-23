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

import oscar.modeling.misc.CartesianProduct
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings._
import oscar.modeling.solvers.cp.distributed.SubProblem
import oscar.modeling.vars.IntVar

/**
  * Iterative deepening decomposition based on
  * @param allVars
  * @param search
  */
class CartProdIterativeDeepening(allVars: List[IntVar], search: BranchingInstantiator) extends IterativeDeepeningStrategy[Double](search) {
  override def initThreshold(model: MemoCPModel, wantedSubProblems: Int): Double = {
    CartesianProduct.computeLog(allVars) - Math.log(wantedSubProblems)
  }

  override def nextThreshold(oldThreshold: Double, currentSubproblems: List[SubProblem], wantedSubProblems: Int): Double = {
    oldThreshold - Math.log(2)
  }

  override def shouldStopSearch(model: MemoCPModel, threshold: Double, depth: Int, discrepancy: Int): Boolean = {
    CartesianProduct.computeLog(allVars) <= threshold
  }
}
