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

package oscar.examples.linprog

import oscar.algebra._
import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.modeling._

import scala.concurrent.Future

/**
 * Model for Sudoku
 */
object Sudoku extends MPModel(LPSolveLib) with App {
  val n = 9

  val N = 0 until n

  val x = Array.tabulate(n, n, n)((l, c, n) => MPIntVar("x" + (l, c, n), 0 to 1))

  maximize(0)

  /* each cell must be assigned exactly one integer */
  for (l <- N; c <- N) // for each row, each column
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(N)((n) => x(l)(c)(n)) === 1)

  /* cells in the same row must be assigned distinct numbers */
  for (l <- N; n <- N)
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(N)((c) => x(l)(c)(n)) === 1)

  /* cells in the same column must be assigned distinct numbers */
  for (c <- N; n <- N)
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(N)((l) => x(l)(c)(n)) === 1)
  /* cells in the same region must be assigned distinct numbers */
  for (l1 <- 0 until 3; c1 <- 0 until 3; n <- N) {
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(0 until 3, 0 until 3)((l, c) => x(l + 3 * l1)(c + 3 * c1)(n)) === 1)
  }

  subjectTo(
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(0)(0)(4) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(0)(1)(2) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(0)(4)(6) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(1)(0)(5) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(1)(3)(0) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(1)(4)(8) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(1)(5)(4) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(2)(1)(8) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(2)(2)(7) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(2)(7)(5) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(3)(0)(7) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(3)(4)(5) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(3)(8)(2) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(4)(0)(3) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(4)(3)(7) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(4)(5)(2) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(4)(8)(0) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(5)(0)(6) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(5)(4)(1) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(5)(8)(5) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(6)(1)(5) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(6)(6)(1) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(6)(7)(7) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(7)(3)(3) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(7)(4)(0) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(7)(5)(8) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(7)(8)(4) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(8)(4)(7) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(8)(7)(6) === 1,
     s"C_${solver.getNumberOfLinearConstraints}" ||: x(8)(8)(8) === 1
  )

  // Run solver in another thread
  import scala.concurrent.ExecutionContext.Implicits.global
  Future {
    solver.solve
  }

  // wait for 1 minute
  Thread.sleep(1 * 60 * 1000)
  // abort
  solver.abort()

  if(solver.hasSolution) {
    println("objective: " + solver.objectiveValue)

    for (l <- N) {
      for (c <- N; n <- N; if x(l)(c)(n).value.get == 1) {
        print(n + 1 + "\t")
      }
      println()
    }
  }

  solver.release()
}
