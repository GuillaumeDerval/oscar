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
