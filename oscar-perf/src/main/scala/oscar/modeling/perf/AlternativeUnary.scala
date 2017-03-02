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

package oscar.modeling.perf

import oscar.modeling._
import oscar.modeling.algebra.integer.Max
import oscar.modeling.constraints.{MaxCumulativeResource, UnaryResource, UnaryResourceSimple}
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar

import scala.util.Random

/**
 * @author Steven Gay steven.gay@uclouvain.be
 * @author Cyrille Dejemeppe cyrille.dejemeppe@uclouvain.be
 */

object AlternativeUnary extends CPApp[Unit] with App {

  val rand = new Random(0)
  val nTasks = 25
  val durationsData = Array.fill(nTasks)(rand.nextInt(10))
  val horizon = durationsData.sum

  val durations = Array.tabulate(nTasks)(t => IntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => IntVar(0, horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val resources = Array.fill(nTasks)(IntVar(0, 1))
  val one = IntVar(1)
  val demands = Array.fill(nTasks)(one)

  val makespan = Max(ends)
  val nResources = 2
  
  // Resources
  for (r <- 0 until nResources) {
    add(UnaryResourceSimple(starts, durations, ends, resources, r))
  }
  
  add(MaxCumulativeResource(starts, durations, ends, demands, IntVar(2)))

  minimize(makespan)

  setSearch {
    Branchings.splitLastConflict(starts ++ resources)
  }

  onSolution {}

  println(solve()._1)
}
