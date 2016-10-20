package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.models.{MemoCPModel, UninstantiatedModel}
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.branchings.Branching._
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

