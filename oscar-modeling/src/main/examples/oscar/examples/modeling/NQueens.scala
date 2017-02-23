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
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar

/**
  * Example of nQueens, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
class NQueens extends CPApp[Unit] {
  override lazy val config = new CPAppConfig {
    val size = trailArg[Int](descr = "Size of the golomb ruler")
  }
  val nQueens = config.size()
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(IntVar(0, nQueens - 1))

  // Constraints
  post(AllDifferent(queens))
  post(AllDifferent(Queens.map(i => queens(i) + i).toArray))
  post(AllDifferent(Queens.map(i => queens(i) - i).toArray))

  setSearch(Branchings.binaryFirstFail(queens))
  onSolution {}

  setDecompositionStrategy(new CartProdRefinement(queens, Branchings.binaryFirstFail(queens)))
  //setDecompositionStrategy(new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(queens)), queens))
  val (stats, solutions) = solve()
  println(stats)
}

object StartNQueens extends NQueens with App {}