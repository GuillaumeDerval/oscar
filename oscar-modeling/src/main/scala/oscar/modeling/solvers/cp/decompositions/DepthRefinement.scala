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

import oscar.modeling.constraints.Constraint
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator

/**
  * A decomposition strategy based on refinement and depth of the subproblem in the tree
  * @param search search to be used
  */
class DepthRefinement(search: BranchingInstantiator) extends RefinementStrategy[Int](search)(scala.math.Ordering.Int.reverse) {
  override def generate(memoCpModel: MemoCPModel, assignment: List[Constraint], path: List[Int]): Int = path.length
}
