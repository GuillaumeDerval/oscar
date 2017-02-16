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

package oscar.examples.modeling

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, ParallelCPSolving, SequentialCPSolving}
import oscar.modeling.solvers.mip.MIPSolving
import oscar.modeling.vars.IntVar

class SendMoreMoney2 extends ModelDeclaration {
  val all = Array.fill(8)(IntVar(0, 9))
  val Array(s, e, n, d, m, o, r, y) = all

  //SEND, MORE and MONEY must no start with a 0
  add(s > 0)
  add(m > 0)

  //All the variables must have a different value
  add(AllDifferent(all))

  //And we have the main constraint
  add(        s*1000 + e*100 + n*10 + d +
              m*1000 + o*100 + r*10 + e ===
    m*10000 + o*1000 + n*100 + e*10 + y)
}

object MySolver extends SolverApp[String]
  with MIPSolving
  with SequentialCPSolving
  with ParallelCPSolving {
  override val modelDeclaration = new SendMoreMoney2()

  val vars = modelDeclaration.all

  //Add a CP search
  setCPSearch(Branchings.binaryStatic(vars))

  //Add an EPS decomposition
  setCPDecompositionStrategy(new CartProdRefinement(vars, Branchings.binaryStatic(vars)))

  //Solution handler
  onSolution { vars.map(_.min).mkString(",") }

  //Display first solution
  println(solve().head)
}