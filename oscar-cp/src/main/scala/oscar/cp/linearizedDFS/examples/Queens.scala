package oscar.cp.linearizedDFS.examples

import oscar.cp._
import oscar.cp.constraints.{AllDiffAC, AllDiffFWC}
import oscar.cp.linearizedDFS.branching.BinaryFirstFailBranching
import oscar.cp.linearizedDFS.{DFSLinearizer, DFSReplayer}

/**
  * Created by saschavancauwelaert on 24/02/16.
  */
object Queens extends CPModel with App {

  val nQueens = 10
  // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

  // Constraints
  add(new AllDiffFWC(queens))
  add(new AllDiffFWC(Queens.map(i => queens(i) + i)))
  add(new AllDiffFWC(Queens.map(i => queens(i) - i)))

  //dfs listener
  val dFSListener = new DFSLinearizer
  solver.searchEngine.searchListener(dFSListener)

  // Search heuristic
  search(new BinaryFirstFailBranching(queens))

  // Execution
  val stats = start()
  println(stats)
  println(dFSListener.searchStateModifications.length)
  println(dFSListener.searchStateModifications(0))

  val replayer = new DFSReplayer(solver, queens)

  //TODO : replaySubjectTo
  add(new AllDiffAC(queens))
  add(new AllDiffAC(Queens.map(i => queens(i) + i)))
  add(new AllDiffAC(Queens.map(i => queens(i) - i)))

  println(replayer.replay(Array(dFSListener.searchStateModifications)))

}