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

import oscar.cp._

import oscar.visual._
import oscar.util._
import scala.io.Source
import oscar.cp.scheduling.visual.VisualGanttChart

/**
 * Job-Shop Problem
 *
 * Created on 03/06/15.
 *
 * A Job is a a sequence of n Activities that must be executed one after the
 * others. There are n machines and each activity of the jobs require one of the
 * n machines. The objective is to assign the starting time of each activity
 * minimizing the total makespan and such that no two activities from two different
 * jobs requiring the same machine overlap.
 *
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object JobShop extends CPModel with App {

  // Parsing
  // -----------------------------------------------------------------------

  var lines = Source.fromFile("../data/ft10.txt").getLines.toList

  val nJobs = lines.head.trim().split(" ")(0).toInt
  val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
  val nResources = lines.head.trim().split(" ")(2).toInt

  val nActivities = nJobs * nTasksPerJob

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  lines = lines.drop(1)

  val jobs = Array.fill(nActivities)(0)
  val resources = Array.fill(nActivities)(0)
  val durations = Array.fill(nActivities)(0)

  for (i <- Activities) {

    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray

    jobs(i) = l(0)
    resources(i) = l(1)
    durations(i) = l(2)

    lines = lines.drop(1)
  }

  // Modeling
  // -----------------------------------------------------------------------

  val horizon = durations.sum

  // Activities & Resources
  val durationsVar = Array.tabulate(nActivities)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nActivities)(t => CPIntVar(0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nActivities)(t => CPIntVar(durationsVar(t).min to horizon))
  val demandsVar = Array.fill(nActivities)(CPIntVar(1))
  val resourcesVar = Array.tabulate(nActivities)(t => CPIntVar(resources(t)))

  val makespan = maximum(endsVar)

  // Constraints & Search
  // -----------------------------------------------------------------------

  // Consistency
  for (t <- Activities) {
    add(endsVar(t) == startsVar(t) + durationsVar(t))
  }
  // Precedences
  for (t <- 1 to Activities.max if jobs(t - 1) == jobs(t)) {
    add(endsVar(t - 1) <= startsVar(t))
  }
  // Cumulative
  val rankBranchings = for (r <- Resources) yield {
    def filter(x: Array[CPIntVar]) = Activities.filter(resources(_) == r).map(x(_))
    val (s,d,e) = (filter(startsVar), filter(durationsVar), filter(endsVar))
    add(unaryResource(s,d,e))
    rank(s,d,e)
  }

  minimize(makespan)

  val rankBranching = rankBranchings.reduce{_++_}

  solver.search {
    rankBranchings.reduce{_++_} ++ binaryStatic(startsVar)
  }
  val stats = start(85)

  println(stats)
}

