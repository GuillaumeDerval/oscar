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

/**
 * Workforce management is central to efficient operations and good customer service.
 * Proper scheduling of employees can mean the difference between profitability and business failure.
 * As the manager of a popsicle stand, you are required to hire and set the weekly work schedule for your employees.
 * The required staffing levels for the week are as follows.
 * Total employees required: Monday=5, Tuesday=7, Wednesday=7, Thursday=10, Friday=16, Saturday=18, Sunday=12.
 * Assume the same staffing requirements continue week after week.
 * Full Time employees work 5 consecutive days and earn $100 per day.
 * Part Time employees work 2 consecutive days and earn $150 per day.
 *
 * Question:  What is the minimal weekly staffing cost you can achieve while meeting the required staffing levels?
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object PopsicleScheduling extends MPModel(LPSolveLib) with App {

  val demand = Array(5, 7, 7, 10, 16, 18, 12)

  def generateConfig(start: Int, length: Int) = {
    val config = Array.fill(7)(0)
    for (j <- 0 until length) {
      config((start + j) % 7) = 1
    }
    config
  }


  val configs =
    Array.tabulate(7)(i => (MPIntVar(s"FTE_$i", 0 to demand.max), 100, generateConfig(i, 5))) ++
    Array.tabulate(7)(i => (MPIntVar(s"PTE_$i", 0 to demand.max), 150, generateConfig(i, 2)))

  minimize(sum(configs)(c => c._1 * (c._2 * c._3.sum)))
  for ((d, i) <- demand.zipWithIndex) {
    println("demand:" + d)
    add( s"C_${solver.getNumberOfLinearConstraints}" ||: sum(configs)(c => c._1 * c._3(i).toDouble) >= d.toDouble)
  }

  solver.solve

  println(solver.objectiveValue)
  for (c <- configs; if (c._1.value.get > 0)) {
    println(c._1.value.get + " X \t" + c._3.mkString("\t"))
  }
  println("--------------")
  val cover = for ((d, i) <- demand.zipWithIndex) yield {
    configs.map(c => c._1.value.get * c._3(i)).sum
  }
  println("\t" + cover.mkString("\t"))

  solver.release()
}
