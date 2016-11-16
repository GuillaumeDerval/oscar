package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.constraints.Constraint
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator

/**
  * A decomposition strategy based on refinement and depth of the subproblem in the tree
  * @param search search to be used
  */
class DepthRefinement(search: BranchingInstantiator) extends RefinementStrategy[Int](search)(scala.math.Ordering.Int.reverse) {
  override def generate(assignment: List[Constraint], path: List[Int]): Int = path.length
}
