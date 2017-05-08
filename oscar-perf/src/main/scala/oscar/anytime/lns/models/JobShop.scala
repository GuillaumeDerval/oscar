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

import oscar.cp._
import oscar.cp.scheduling.visual.VisualGanttChart
import oscar.anytime.lns.Benchmark
import oscar.util.RandomGenerator
import oscar.visual._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
 * Job-Shop Problem
 *
 *  A Job is a a sequence of n Activities that must be executed one after the
 *  others. There are n machines and each activity of the jobs require one of the
 *  n machines. The objective is to assign the starting time of each activity
 *  minimizing the total makespan and such that no two activities from two different
 *  jobs requiring the same machine overlap.
 *
 *  @author Pierre Schaus  pschaus@gmail.com
 */
class JobShop(val instance: String, val bestObj: Int = Int.MaxValue) extends Benchmark {

  // Parsing    
  // -----------------------------------------------------------------------

  var lines = Source.fromFile(instance).getLines().toList

  lines = lines.drop(3)
  val l1 = lines(0).trim().split(" ").map(_.toInt)
  lines = lines.drop(1)
  println(l1.mkString(","))


  val nJobs = l1(0)
  val nTasksPerJob = l1(1)
  val nResources = l1(1)

  val nActivities = nJobs * nTasksPerJob

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  val jobs = Array.fill(nActivities)(0)
  val resources = Array.fill(nActivities)(0)
  val durations = Array.fill(nActivities)(0)

  var i = 0
  for (j <- 0 until nJobs) {

    val (resource,duration) = lines(0).trim().split("\\s+").map(_.toInt).sliding(2,2).toArray.map(a => (a(0),a(1))).unzip
    for (r <- 0 until nResources) {
      resources(i) = resource(r)
      durations(i) = duration(r)
      jobs(i) = j
      i += 1
    }
    lines = lines.drop(1)

  }


  // Modeling 
  // -----------------------------------------------------------------------

  implicit val cp = CPSolver()
  val horizon = durations.sum

  // Activities & Resources
  val durationsVar = Array.tabulate(nActivities)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nActivities)(t => CPIntVar(0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nActivities)(t => startsVar(t)+durations(t))
  val demandsVar = Array.fill(nActivities)(CPIntVar(1))
  val resourcesVar = Array.tabulate(nActivities)(t => CPIntVar(resources(t)))

  val makespan = maximum(endsVar)

  // Constraints & Search
  // -----------------------------------------------------------------------

  // Precedences
  for (t <- 1 to Activities.max if jobs(t - 1) == jobs(t)) {
    add(endsVar(t - 1) <= startsVar(t))
  }

  // Unary Resources
  for (r <- Resources) yield {
    def filter(x: Array[CPIntVar]) = Activities.filter(resources(_) == r).map(x(_))
    val (s,d,e) = (filter(startsVar), filter(durationsVar), filter(endsVar))
    add(unaryResource(s,d,e))
  }

  minimize(makespan) 


  def decisionVariables: Array[CPIntVar] = startsVar

  def bestKnownObjective: Int = bestObj

  def objective: CPIntVar = makespan


}


