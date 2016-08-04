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
import Migration._

import scala.io.Source

/**
 * Uncapacitated facility location problem.
 *
 * A series of logs to cut and ship to a set of landings.
 * One must decide which logs to cut, which landings to open,
 * given shipment and landing opening costs
 * Cost of shipment of each log to each landing are known.
 * A minimal number of logs should be cut, modeled by a
 * trade-off coefficient and an additional term in the objective function.
 *
 * @author Bertrand Cornelusse
 */
object LandingLocation extends MPModel(LPSolveLib) with App {

  // ---------- Data of the Problem ----------

  val dataIterator = Source.fromFile("data/LandingLocation.txt").getLines()
  val openingCost = for (el <- dataIterator.next().split(";")) yield el.toDouble
  val transportationCost = (for (line <- dataIterator) yield for (el <- line.split(";").drop(1)) yield el.toDouble).toArray
  val Landings = openingCost.indices
  val Logs = transportationCost.indices
  val Alpha = 700 // A trade-off coefficient
  val Demand = 4

  // ---------- MIP model ----------

  // Variables
  // For each landing either it is ON or OFF
  val y = for (land <- Landings) yield MPIntVar("y" + land, 0 to 1)

  // Transportation decision variables
  val x = Array.tabulate(Logs.length, Landings.length)((log, land) => MPIntVar("x" + (log, land), 0 to 1))

  val obj =
    sum(Logs, Landings) { (log, land) => x(log)(land) * transportationCost(log)(land).toDouble } +
    sum(Landings) { land => y(land)*openingCost(land).toDouble } +
     (Const(Demand.toDouble) - sum(Logs, Landings) { (log, land) => x(log)(land) })*Alpha.toDouble

  minimize(obj)

  // One log can be assigned only to one landing
  for (log <- Logs) {
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(Landings) { (land) => x(log)(land) } <= 1.0)
  }
  // One log can be assigned to a landing only if that landing is open
  for (log <- Logs; land <- Landings) {
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: x(log)(land) <= y(land))
  }

  solver.solve
  println("objective: " + solver.objectiveValue)
  println("----------")
  println(y.mkString("\n"))
  x.foreach(log => println(log.map(_.value.get).mkString("\t")))
  x.foreach(log => println(log.map(_.value.get).mkString("\t")))

  solver.release()
}
