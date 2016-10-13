package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.CartesianProduct
import oscar.modeling.solvers.cp.{SubProblem, SubProblemCartesianProductLog}
import oscar.modeling.solvers.cp.branchings.Branching.BranchingInstantiator
import oscar.modeling.vars.IntVar

/**
  * A decomposition strategy based on refinement and cartesian product size
  * @param allVars important variables that will be taken into account to compute the cartesian product log
  * @param search search to be used
  */
class CartProdRefinement(allVars: Iterable[IntVar], search: BranchingInstantiator) extends RefinementStrategy[Double](search) {
  override def generate(assignment: List[Constraint], path: List[Int]): Double = CartesianProduct.computeLog(allVars)
  override def extendSubProblem(subproblem: SubProblem, cartProd: Double): SubProblem = subproblem addData(SubProblemCartesianProductLog, cartProd)
}
