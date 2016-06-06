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

package oscar.examples.cp

import oscar.algo.search.DFSLinearizer
import oscar.cp._

/**
  * @author Sascha Van Cauwelart
  * @author Pierre Schaus pschaus@gmail.com
  */
object QueensReplay extends  App {

  implicit val cp = CPSolver()

  val nQueens = 8
  // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

  // Search heuristic
  search(binaryFirstFail(queens))

  val allDiffs = Seq(allDifferent(queens),
    allDifferent(Queens.map(i => queens(i) + i)),
    allDifferent(Queens.map(i => queens(i) - i)))



  val linearizer = new DFSLinearizer()


  cp.onSolution()

  // Execution with FC allDifferent
  val statsInit = startSubjectTo(searchListener = linearizer) {
    add(allDiffs,Weak)
  }

  // Replay with AC allDifferent
  val statsReplayAC = cp.replaySubjectTo(linearizer, queens, println(queens.mkString(" "))) {
    add(allDiffs,Strong)
  }

  println(statsInit)
  println(statsReplayAC)



}