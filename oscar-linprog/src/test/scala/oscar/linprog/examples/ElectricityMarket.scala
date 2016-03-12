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

package oscar.linprog.examples

import oscar.algebra._
import oscar.linprog.interface.MIPSolverInterface
import oscar.linprog.interface.lpsolve.LPSolveLib
import oscar.linprog.modeling._

import scala.io.Source

/**
 * Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 *
 * Maximize the total market exchange such that demand and supply match at any time.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElectricityMarket extends MPModel(LPSolveLib) with App {

  // format is : qty ( > 0 if producer < 0 if consumer) start end
  val firstLine :: restLines = Source.fromFile("data/electricityMarketEasy.txt").getLines().toList
  val n = firstLine.toInt

  val orders = restLines.map(_.split(" ").map(_.toInt))
  val producers = orders.filter(_(0) > 0)
  val consumers = orders.filter(_(0) < 0)

  val tmin = orders.map(_(1)).min
  val tmax = orders.map(_(2)).max

  // one variable for each order if we take it or not
  var varMap = Map[Array[Int], MPVar[_ <: MIPSolverInterface]]()
  orders.foreach(o => varMap += (o -> MPIntVar(s"order_$o", 0 to 1)))

  // helper functions
  def overlap(order: Array[Int], t: Int) = t <= order(2) && t >= order(1)
  def vars(l: List[Array[Int]], t: Int) = l.filter(overlap(_, t)).map(varMap(_)).toArray
  def qty(l: List[Array[Int]], t: Int) = l.filter(overlap(_, t)).map(_(0).abs).toArray

  val t0 = System.currentTimeMillis

  maximize(sum(producers)(order => varMap(order) * (order(0) * (order(2) - order(1) + 1))))

  for (t <- tmin to tmax) {
    val prod_t = producers.filter(overlap(_, t))
    val cons_t = consumers.filter(overlap(_, t))
    if (prod_t.nonEmpty && cons_t.nonEmpty) {
      // production and consumption must be the same at time t
      add(sum(prod_t)(p => varMap(p) * p(0).abs) =:= sum(cons_t)(c => varMap(c) * c(0).abs))
    }
  }

  solver.solve

  println("time:" + (System.currentTimeMillis - t0))
  println("objective:" + solver.objectiveValue)
  val check = Array.tabulate(tmax - tmin + 1)(_ => 0)
  for (o <- orders; if varMap(o).value.get > 0.5) {
    for (t <- o(1) to o(2)) {
      check(t - 1) += o(0)
    }
  }

  solver.release()
}
