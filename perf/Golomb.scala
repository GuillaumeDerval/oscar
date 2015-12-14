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

import oscar.cp._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object Golomb extends CPModel with App {

  solver.silent = true

  // Data
  val m = 11
  val n = m * m

  // Variables
  val mark = Array.fill(m)(CPIntVar(0 to n))

  val differences = for { i <- 0 until m; j <- i + 1 until m } yield {
    val d = mark(j) - mark(i) // inefficient because it does not create a sparse one      
    add(d >= (j - i) * (j - i + 1) / 2)
    d
  }

  minimize(mark(m - 1))

  add(allDifferent(mark), Medium)
  add(allDifferent(differences), Medium)

  for (i <- 1 until mark.length) add(mark(i - 1) <= mark(i), Strong)

  // symmetry breaking
  add(mark(0) == 0)

  add(differences(0) <= differences((m * m - m) / 2 - 1))

  // ensure positive differences 
  // (Cred to Pierre Schaus.)
  differences.foreach(d => add(d > 0))

  search { binaryStatic(mark, _.min) }
  /*
  onSolution {
    println("\nSolution:")
    print("mark: " + mark.mkString(""))
    println("\ndifferences: " + differences.mkString(","))
    println()
  }*/

  println(start())
}


