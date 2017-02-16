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