package oscar.cp.lineardfs.examples

import oscar.cp._
import oscar.cp.constraints.{AllDiffAC, AllDiffFWC}

/**
  * Created by saschavancauwelaert on 24/02/16.
  */
object Queens extends CPModel with App {

  val nQueens = 10
  // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

  // Search heuristic
  search(binaryFirstFail(queens))

  //set up listener
  listen()

  // Execution
  val stats = startSubjectTo() {
    // Constraints
    add(new AllDiffFWC(queens))
    add(new AllDiffFWC(Queens.map(i => queens(i) + i)))
    add(new AllDiffFWC(Queens.map(i => queens(i) - i)))
  }

  //extend the model with additional constraints
  add(new AllDiffAC(queens))
  add(new AllDiffAC(Queens.map(i => queens(i) + i)))
  add(new AllDiffAC(Queens.map(i => queens(i) - i)))

  //replay
  val stat2 = replay(queens)

  println(stats)
  println(stat2)

}