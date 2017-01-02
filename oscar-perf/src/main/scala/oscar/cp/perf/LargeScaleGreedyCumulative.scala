package oscar.cp.perf

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

import scala.io.Source

/** @author Pierre Schaus */
object LargeScaleGreedyCumulative extends CPModel with App {

  val lines = Source.fromFile("data/large_scale_scheduling_01.txt").getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nTasks = next()
  val capa = next()
  
  val durations = Array.fill(nTasks)(next())
  val height = Array.fill(nTasks)(next())
  val horizon = durations.sum

  
  val durationsVar = Array.tabulate(nTasks)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPIntVar(0, horizon))
  val endsVar = Array.tabulate(nTasks)(t => startsVar(t) + durationsVar(t))
  val demandsVar = Array.tabulate(nTasks)(t => CPIntVar(height(t)))
  val capacity = CPIntVar(capa)

  add(maxCumulativeResource(startsVar, durationsVar, endsVar, demandsVar, capacity), Weak)

  val t0 = System.currentTimeMillis

  
  onSolution {
    println("max=" + endsVar.map(_.value).max)
  }
  
  search {
    binaryStatic(startsVar, _.min)
  }
  val stat = start(nSols = 1)
  println(stat)
  val time = System.currentTimeMillis - t0

  println("time:"+time)
  
}
