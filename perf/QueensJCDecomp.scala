/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import oscar.cp._
import oscar.algo.search.Branching

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensJCDecomp extends CPModel with App {

  solver.silent = true

  val nQueens = 88 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))
  val rQueens = Array.tabulate(nQueens)(i => queens(i) + i)
  val fQueens = Array.tabulate(nQueens)(i => queens(i) - i)

  // Constraints
  for (i <- Queens; j <- Queens; if j < i) {
    add(queens(i) !== queens(j))
    add(rQueens(i) !== rQueens(j))
    add(fQueens(i) !== fQueens(j))
  }

  // Search heuristic
  search(binaryFirstFail(queens))

  // Execution
  val stats = start(nSols = 1)
  println(stats)
  println(solver.statistics)
}


