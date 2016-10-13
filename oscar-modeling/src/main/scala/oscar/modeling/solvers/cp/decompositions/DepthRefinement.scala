package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.CartesianProduct
import oscar.modeling.solvers.cp.branchings.Branching.BranchingInstantiator
import oscar.modeling.solvers.cp.{SubProblem, SubProblemCartesianProductLog}
import oscar.modeling.vars.IntVar

/**
  * A decomposition strategy based on refinement and depth of the subproblem in the tree
  * @param search search to be used
  */
class DepthRefinement(search: BranchingInstantiator) extends RefinementStrategy[Int](search)(scala.math.Ordering.Int.reverse) {
  override def generate(assignment: List[Constraint], path: List[Int]): Int = path.length
}
