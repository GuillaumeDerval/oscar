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

package oscar.modeling.solvers.cp

import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.solvers.cp.distributed.DecomposedCPSolve

/**
  * Stores a CP Decomposition, or redirect to the ModelDeclaration if it implements DecomposedCPSolve
  */
trait CPDecompositionHolder {
  val md: ModelDeclaration
  private var _cpDecomp: DecompositionStrategy = null
  def setCPDecompositionStrategy(d: DecompositionStrategy): Unit = _cpDecomp = d
  def getCPDecompositionStrategy: DecompositionStrategy = {
    if(_cpDecomp == null && md.isInstanceOf[DecomposedCPSolve[_]])
      md.asInstanceOf[DecomposedCPSolve[_]].getDecompositionStrategy
    else
      _cpDecomp
  }
}
