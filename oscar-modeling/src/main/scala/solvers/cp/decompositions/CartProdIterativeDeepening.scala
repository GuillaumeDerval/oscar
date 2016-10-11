package solvers.cp.decompositions

import misc.CartesianProduct
import models.MemoCPModel
import solvers.cp.SubProblem
import solvers.cp.branchings.Branching._
import vars.IntVar

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
