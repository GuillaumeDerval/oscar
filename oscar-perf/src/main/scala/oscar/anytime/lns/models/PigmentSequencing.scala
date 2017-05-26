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

package oscar.anytime.lns.models


import oscar.anytime.lns.Benchmark
import oscar.cp._
import oscar.cp.core.CPSolver

import scala.io.Source


/**
  * @author Pierre Schaus pschaus@gmail.com
  * @author Ratheil Houdji
  */
class PigmentSequencing(val instance: String, val bestObj: Int = Int.MaxValue) extends Benchmark {

  implicit val cp = CPSolver()
  //cp.silent = true
  val lines = Source.fromFile(instance).getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  //  println(vals)
  val nPeriods = next()
  val nItems = next()
  val nDemands = next()

  val changeOverCost = Array.tabulate(nItems, nItems) { case (i, j) => next }
  val h = Array.fill(nItems)(next)


  val demandsByItem = Array.fill(nItems, nPeriods)(next)

  //val nbDemands = demandsByItem.flatten.filter(_ == 1).size
  val stockingCostItems: Array[Int] = Array.fill(nDemands)(0)
  val deadline: Array[Int] = Array.fill(nDemands)(0)
  val item: Array[Int] = Array.fill(nDemands)(0)
  //val nDemandsByItem = Array.fill(nItems)(0)
  var d = 0
  for (i <- 0 until nItems; t <- 0 until nPeriods; if (demandsByItem(i)(t) == 1)) {
    deadline(d) = t
    item(d) = i
    //  nDemandsByItem(i) += 1
    stockingCostItems(d) = h(i)
    d += 1
  }

  val costMatrixChangeover = Array.tabulate(nDemands + 1, nDemands + 1) {
    case (i, j) =>
      if (i == nDemands || j == nDemands) 0
      else changeOverCost(item(i))(item(j))
  }
  //***********************************

  val date: Array[CPIntVar] = Array.tabulate(nDemands)(d => CPIntVar((0 to nPeriods).filterNot(t => t > deadline(d)))(cp)) :+ CPIntVar(1 to nPeriods)(cp)
  val objStock = CPIntVar(0 until 1000000)(cp)

  var nbSols = 0

  var best = Array.fill(nDemands)(0)
  var bestCost = Integer.MAX_VALUE

  val successors = Array.tabulate(nDemands + 1)(i => CPIntVar(0 to nDemands)(cp))
  val preds = Array.tabulate(nDemands + 1)(i => CPIntVar(0 to nDemands)(cp))

  val objChangeOver = CPIntVar(0 until changeOverCost.flatten.max * nPeriods)(cp)

  cp.add(minCircuit(successors,costMatrixChangeover, objChangeOver, false), Weak);


  for (d <- 0 until nDemands) {
    cp.add(date(d) < date(successors(d)))
  }

  for (d <- 0 until (nDemands - 1); if (item(d) == item(d + 1))) {
    cp.add(date(d) < date(d + 1))
  }
  for (n1 <- 0 until (nDemands - 1); n2 <- n1 until nDemands; if (item(n1) == item(n2))) {
    cp.add(successors(n2) !== n1)
  }

  cp.add(stockingCost(date.filterNot(_ == date(nDemands)), deadline, stockingCostItems, objStock, Array.fill((nPeriods + 1))(1)))


  cp.add(objChangeOver === sum(0 until nDemands)(i => costMatrixChangeover(i)(successors(i))))
  val obj  = objChangeOver + objStock

  cp.minimize(obj)

  def decisionVariables: Array[CPIntVar] = date

  override def bestKnownObjective: Int = bestObj

  override def solver: CPSolver = cp

  override def problem: String = "PSP"
}
