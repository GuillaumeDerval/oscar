package examples

import constraints.AllDifferent
import models.UninstantiatedModel
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening, DepthRefinement}
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar
import visualisation.ConstraintsVisualisation

/**
  * Example of nQueens, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
class NQueens extends DistributedCPApp[Unit] {
  override lazy val config = new DistributedCPAppConfig {
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

  setSearch(Branching.binaryFirstFail(queens))
  onSolution {}

  setDecompositionStrategy(new CartProdRefinement(queens, Branching.binaryFirstFail(queens)))
  //setDecompositionStrategy(new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(queens)), queens))
  val (stats, solutions) = solve()
  println(stats)
}

object StartNQueens extends NQueens with App {}