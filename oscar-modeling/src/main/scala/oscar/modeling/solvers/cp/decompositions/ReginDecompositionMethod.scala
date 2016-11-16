package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.solvers.cp.Branchings
import oscar.modeling.solvers.cp.distributed.SubProblem
import oscar.modeling.vars.IntVar

import scala.util.Random

/**
  * The algorithm described by Regin et al. in their paper on Embarrassingly Parallel Search
  *
  * @param vars
  */
class ReginDecompositionMethod(vars: Array[IntVar]) extends DepthIterativeDeepening(Branchings.naryStatic(vars)) {
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    Random.shuffle(super.decompose(baseModel, count))
  }
}
