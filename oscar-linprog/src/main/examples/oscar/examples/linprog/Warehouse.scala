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

import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.modeling._
import oscar.algebra._

/**
 * Capacitated Facility Location Problem
 *
 * There is
 * - a set of plants that can be open or not having a capacity
 * - a set of warehouses
 *
 *
 * Each warehouse has a demand that must be satisfied by one or more plant.
 * There is a cost specified
 * - for shipping one unit from a particular plant to a particular warehouse.
 * - for opening a plant
 *
 * The objective is to minimize the total cost while
 * satisfying the demand of the warehouses and the capacities of the plant
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Warehouse extends MPModel(LPSolveLib) with App {

  // ----------- Data of the problem ------------

  // Warehouse demand in thousands of units
  val demand = Array(15, 18, 14, 20)
  // Plant capacity in thousands of units
  val capacity = Array(20, 22, 17, 19, 18)
  // Fixed costs for each plant
  val fixedCosts = Array(12000, 15000, 17000, 13000, 16000)
  // Transportation costs per thousand units
  val transCosts = Array(Array(4000, 2000, 3000, 2500, 4500),
    Array(2500, 2600, 3400, 3000, 4000),
    Array(1200, 1800, 2600, 4100, 3000),
    Array(2200, 2600, 3100, 3700, 3200))
  // Number of plants and warehouses
  val Plants = 0 until capacity.length
  val Warehouses = 0 until demand.length

  // ----------- MIP Model ------------


  // For each plant whether it is open (1) or not (0)
  val open = Plants.map(p => MPBinaryVar("open" + p))

  // Transportation decision variables: how much to transport from a plant p to a warehouse w
  val transport = Array.tabulate(Warehouses.length, Plants.length)((w, p) => MPFloatVar("trans" + (w, p), lb = 0))

  // The objective is to minimize the total fixed and variable costs
  minimize(sum(Warehouses, Plants) { (w, p) => transport(w)(p) * transCosts(w)(p) } //variable cost
    + sum(Plants) { p => open(p) * fixedCosts(p) }) //fixed costs
  // Production Constraints
  for (p <- Plants) {
    add(sum(Warehouses)(w => transport(w)(p)) <:= capacity(p) * open(p))
  }
  // Demand Constraints
  for (w <- Warehouses) {
    add(sum(Plants)(p => transport(w)(p)) >:= demand(w))
  }

  solver.solve

  println("objective: " + solver.objectiveValue)
  println("----------")

  open.foreach { o =>
    println(o+" "+o.value.get)
  }
  transport.foreach(x => println(x.map(_.value.get).mkString("\t")))

  solver.release()
}
