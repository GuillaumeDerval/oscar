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
  *******************************************************************************/


import oscar.cp._
import oscar.util._

/**
  * Type of Traveling Salesman: find shortest tour of 10 cities among 20 possible cities
  * Given a distance matrix between 20 cities,
  * find the shortest tour visiting each city exactly once.
  *
  * @author Pierre Schaus  pschaus@gmail.com
  */
object SubCircuit extends CPModel with App {
  // Data
  val nCities = 20
  val Cities = 0 until nCities
  val (distMatrix, coordinates) = TSPGenerator.randomInstance(nCities)

  for (i <- 0 until nCities) {
    println(distMatrix(i).mkString(","))
  }

  // Variables
  val succ = Array.fill(nCities)(CPIntVar(Cities))
  val totDist = CPIntVar(0 to distMatrix.flatten.sum)
  add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)

  val nEdge = sum(Cities)(i => succ(i) !== i)
  add(nEdge >= 12)

  // Constraints
  add(subCircuit(succ), Weak)

  // Search heuristic
  minimize(totDist)

  search {
    // Select the not yet bound city with the smallest number of possible successors
    selectMin(Cities)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get
        branch(add(succ(x) === v))(add(succ(x) !== v))
      }
    }
  }

  var nSols = 0
  onSolution {
    nSols += 1
  }

  println(start())
}
