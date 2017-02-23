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

import oscar.algo.search.{Alternative => _, _}
import oscar.cp._
import oscar.cp.constraints.{CPObjective, CPObjectiveUnitMinimize}
import oscar.util._

/**
  * @author Sascha Van Cauwelaert
  */
object ProductMatrixTSP extends App {

  val NO_EDGE = 400
  val FAKE_WEIGHT = 100

  val fileURI = "data/atsp-200-0.tsp"


  val (matrix, consumptions, costs) = readInstanceForCP(fileURI)

  val nVar = matrix.length
  val maxAssignment = nVar

  implicit val solver = CPSolver()
  solver.silent = false

  val x = Array.tabulate(nVar)(v => CPIntVar(0 to maxAssignment))

  val maxProduct = CPIntVar(0 until Int.MaxValue)
  val objective = new CPObjectiveUnitMinimize(maxProduct)
  val cpObjective = new CPObjective(solver, objective)
  solver.optimize(cpObjective)


  val br = conflictOrderingSearch(x, i => (x(i).min, i), learnValueHeuristic(x, i => x(i).min))

  search(br ++ binaryStatic(Array(maxProduct)))

  val pricesForVariable: Array[Array[Int]] = matrix

  val individualConsumptions = Array.tabulate(x.length)(i => element(pricesForVariable(i), x(i)))
  post(maxProduct === sum(individualConsumptions))

  var optimum = Int.MaxValue
  solver.onSolution {
    optimum = maxProduct.value
  }

  post(circuit(x, false))
  post(minAssignment(x, pricesForVariable, maxProduct), Weak)

  val stats = solver.start()


  println(s"optimum : $optimum")
  println(s"optimum condor : ${optimum + FAKE_WEIGHT * pricesForVariable.length}")

  println(stats)


  def readInstanceForCP(fileURI: String): (Array[Array[Int]], Array[Int], Array[Int]) = {
    val lines: Array[String] = scala.io.Source.fromFile(fileURI).getLines().toArray
    val consumptions = lines(3).replace("Consumptions: ", "").split(" ").map(_.toInt)
    val prices = lines(4).replace("Prices: ", "").split(" ").map(_.toInt)
    val fullMatrix = lines.drop(10).map(l => l.split(" ").map(_.toInt))
    val doubleDimension = fullMatrix.length
    val realDimension = doubleDimension / 2
    val extractedMatrix = fullMatrix.drop(realDimension).map(_.dropRight(realDimension))
    val matrix = (0 until realDimension).map(i => (0 until realDimension).map(j => if (i == j) 0 else extractedMatrix(i)(j) - FAKE_WEIGHT).toArray).toArray
    (matrix, consumptions, prices)
  }


}
