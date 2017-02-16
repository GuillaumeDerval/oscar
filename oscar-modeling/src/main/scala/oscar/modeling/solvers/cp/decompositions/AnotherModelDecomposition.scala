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
