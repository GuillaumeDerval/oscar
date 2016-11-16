package oscar.modeling.examples

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening, DepthRefinement}
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar
import oscar.modeling.visualisation.ConstraintsVisualisation

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