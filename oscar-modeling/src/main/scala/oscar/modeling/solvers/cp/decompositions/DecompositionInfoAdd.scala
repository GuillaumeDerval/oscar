/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.misc.CartesianProduct
import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemCartesianProductLog}
import oscar.modeling.vars.IntVar

/**
  * Decomposition that uses another decomposition, but add infos to subproblems.
  * @param baseDecomposition
  */
abstract class DecompositionInfoAdd(baseDecomposition: DecompositionStrategy) extends DecompositionStrategy {
  /**
    * Decompose the problem
    *
    * @param baseModel the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of subproblems
    */
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    val model = new MemoCPModel(baseModel.removeOptimisation())
    model.apply {
      baseDecomposition.decompose(baseModel, count).map((sp) => {
        model.pushState()
        for(c <- sp.constraints)
          model.post(c)
        val out = addInfo(sp, model)
        model.popState()
        out
      })
    }
  }

  /**
    * Add info to a problem
    *
    * @param subProblem
    * @return the same subProblem, but with additionnal info
    */
  def addInfo(subProblem: SubProblem, model: MemoCPModel): SubProblem
}

class DecompositionAddCartProdInfo(baseDecomposition: DecompositionStrategy, allVars: Iterable[IntVar]) extends DecompositionInfoAdd(baseDecomposition) {
  /**
    * Add info to a problem
    *
    * @param subProblem
    * @return the same subProblem, but with additionnal info
    */
  override def addInfo(subProblem: SubProblem, memoCPModel: MemoCPModel): SubProblem = {
    subProblem.addData(SubProblemCartesianProductLog, CartesianProduct.computeLog(allVars))
  }
}