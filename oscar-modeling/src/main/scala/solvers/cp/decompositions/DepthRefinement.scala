package solvers.cp.decompositions

import constraints.Constraint
import misc.CartesianProduct
import solvers.cp.branchings.Branching.BranchingInstantiator
import solvers.cp.{SubProblem, SubProblemCartesianProductLog}
import vars.IntVar

/**
  * A decomposition strategy based on refinement and depth of the subproblem in the tree
  * @param search search to be used
  */
class DepthRefinement(search: BranchingInstantiator) extends RefinementStrategy[Int](search)(scala.math.Ordering.Int.reverse) {
  override def generate(assignment: List[Constraint], path: List[Int]): Int = path.length
}
