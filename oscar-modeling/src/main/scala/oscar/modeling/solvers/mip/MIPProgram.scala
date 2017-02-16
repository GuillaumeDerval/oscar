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

package oscar.modeling.solvers.mip

import de.xypron.linopt.SolverGlpk
import oscar.modeling.models.mip.MIPModel
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy, UninstantiatedModel}
import oscar.modeling.solvers.SolveHolder

class MIPProgram[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration()) extends SolveHolder[RetVal] with ModelDeclarationProxy {
  implicit val program = this
  override implicit val md = modelDeclaration
  val solver = new SolverGlpk()

  def solve(): Option[RetVal] = {
    val umodel = md.getCurrentModel.asInstanceOf[UninstantiatedModel]
    val model = new MIPModel(umodel)
    md.apply(model){
      val out = solver.solve(model.linProblem)

      if(out) {
        model.hasSolution = true
        val out = Some(onSolution())
        model.hasSolution = false
        out
      }
      else
        None
    }
  }

  override protected val solveRedirectTo: Any = md
}