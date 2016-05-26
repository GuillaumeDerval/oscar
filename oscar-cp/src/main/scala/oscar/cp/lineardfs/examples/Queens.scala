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

package oscar.cp.lineardfs.examples

import oscar.cp._
import oscar.cp.constraints.{AllDiffAC, AllDiffFWC}

/**
  * @author Sascha Van Cauwelart
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