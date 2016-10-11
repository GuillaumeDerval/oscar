package solvers.cp.decompositions

import models.UninstantiatedModel
import solvers.cp.SubProblem
import solvers.cp.branchings.Branching
import vars.IntVar

import scala.util.Random

/**
  * The algorithm described by Regin et al. in their paper on Embarrassingly Parallel Search
  *
  * @param vars
  */
class ReginDecompositionMethod(vars: Array[IntVar]) extends DepthIterativeDeepening(Branching.naryStatic(vars)) {
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    Random.shuffle(super.decompose(baseModel, count))
  }
}
