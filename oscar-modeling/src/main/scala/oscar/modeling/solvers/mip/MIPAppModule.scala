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

import org.rogach.scallop.Subcommand
import oscar.modeling.solvers.{SolveHolder, SolverApp, SolverAppModule}

/**
 * Module for SolverApp that solves models using a LP solver
 * @param app the SolverApp
 */
class MIPAppModule(app: SolverApp[_]) extends SolverAppModule {
  class SequentialCPSubcommand extends Subcommand("mip") {
    descr("Solves the model using a MIP solver.")
  }
  override val subcommand = new SequentialCPSubcommand

  override def solve[RetVal](): List[RetVal] = {
    val pg = new MIPProgram[RetVal](app.modelDeclaration)
    val onSolution: () => RetVal = app.asInstanceOf[SolveHolder[RetVal]].onSolution
    if(onSolution == null)
      throw new RuntimeException("No onSolution defined in the SolverApp or in the ModelDeclaration")
    pg.onSolution{onSolution()}

    val result = pg.solve()
    result.toList
  }
}