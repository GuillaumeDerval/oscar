package solvers.cp.decompositions

import models.UninstantiatedModel
import solvers.cp.{SubProblem}

trait DecompositionStrategy {
  /**
    * Decompose the problem
    *
    * @param model the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of subproblems
    */
  def decompose(model: UninstantiatedModel, count: Int): List[SubProblem]
}

class NoDecompositionStrategy extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Int): List[SubProblem] = {
    List(new SubProblem(List()))
  }
}