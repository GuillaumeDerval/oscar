package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.solvers.cp.distributed.SubProblem

/**
  * Decompose using another model
  * @param model
  * @param decomp
  */
class AnotherModelDecomposition(model: UninstantiatedModel, decomp: DecompositionStrategy) extends DecompositionStrategy{
  /**
    * Decompose the problem
    *
    * @param unused the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of subproblems
    */
  override def decompose(unused: UninstantiatedModel, count: Int): List[SubProblem] = decomp.decompose(model, count)
}
