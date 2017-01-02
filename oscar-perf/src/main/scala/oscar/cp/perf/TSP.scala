package oscar.cp.perf

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
import oscar.util._

import scala.io.Source

/**
 *
 * tsp model: given a distance matrix between 20 cities,
 *            find the shortest tour visiting each city exactly once.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object TSP extends CPModel with App {

  val n = 58
  val Cities = 0 until n
  val lines = Source.fromFile("data/tsp.txt").getLines.toList

  val distMatrix = lines.grouped(n).map(i => i.map(j => j.toInt).toArray).toArray

  solver.silent = true
  
  // Successors
  val succ = Array.tabulate(n)(_ => CPIntVar.sparse(0, n))
  
  // Total distance
  val dist = CPIntVar(0, distMatrix.flatten.sum)

  // Constraints
  add(minCircuit(succ, distMatrix, dist), Strong)

  minimize(dist)

  search {
    // Select the not yet bound city with the smallest number of possible successors
    selectMin(Cities)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get
        branch(post(succ(x) === v))(post(succ(x) !== v))
      }
    }
  }
  
  println(start())
}
