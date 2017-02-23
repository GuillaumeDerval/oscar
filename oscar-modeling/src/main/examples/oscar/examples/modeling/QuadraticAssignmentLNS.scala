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

package oscar.examples.modeling

import scala.io.Source
import oscar.modeling.algebra.integer.Sum
import oscar.modeling.constraints.{AllDifferent, StrongPropagation}
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPSolver}
import oscar.modeling.vars.IntVar

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Guillaume Derval guillaume.derval@uclouvain.be
 */
object QuadraticAssignmentLNS extends ModelDeclaration with App {

  // Read the data
  var lines = Source.fromFile("data/qap.txt").getLines.toList.filter(_ != "")
  val n = lines.head.toInt
  val N = 0 until n
  lines = lines.drop(1)
  var w: Array[Array[Int]] = Array() //weight matrix
  var d: Array[Array[Int]] = Array() //distance matrix
  for (i <- N) {
    w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_.toInt).toArray
    lines = lines.drop(1)
  }
  for (i <- N) {
    d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_.toInt).toArray
    lines = lines.drop(1)
  }


  // for each facilities, the location chosen for it
  val x = N.map(v => IntVar(0 until n)).toArray

  add(new AllDifferent(x) with StrongPropagation)

  minimize(Sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j)))

  val solver = CPSolver[Unit]().use()

  solver.setSearch(Branchings.binaryFirstFail(x))

  var lastSol: Array[Int] = _
  solver.onSolution {
    lastSol = x.map(_.min)
    println(lastSol.mkString(","))
  }

  // Search for an initial solution
  solver.solve(nSols=1)

  val rand = new scala.util.Random(0)
  var limit = 1000 // set the timeout to 1 sec

  for (r <- 1 to 200) {
    // relax randomly 50% of the variables and run again

    val stat = solver.solveSubjectTo(maxTime = limit) {
      add(N.filter(i => rand.nextInt(100) < 50).map(i => x(i) === lastSol(x(i))).map(_.toConstraint))
    }

    // adapt the time limit for next run *2 is previous run reached the limit /2 otherwise
    limit = if (stat._1.completed) limit / 2 else limit * 2
    println("set limit to " + limit)
  }
  println(lastSol.mkString(","))
}
