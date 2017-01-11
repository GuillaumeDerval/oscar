/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.business.scheduling.algo

import oscar.cbls.business.scheduling.model.{Activity, Planning}
import oscar.cbls.lib.search.LinearSelector

/**
 * finds a critical path for the given planning
 * @author renaud.delandtsheer@cetic.be
 */
object CriticalPathFinder extends LinearSelector {

  /**
   * returns a critical path in the planning
   *
   * @param p the planning
   * @return a list of activities that is a critical path for the planning
   */
  def criticalPath(p: Planning)(from: Activity = p.sentinelActivity): List[Activity] = {
    def PrecedingNode(j: Activity): Activity = {
      if (j.definingPredecessors.value.isEmpty) null
      else p.activityArray(selectFrom(j.definingPredecessors.value))
      //random tie break, as it is likely that there will be few forks.
    }

    var CurrentActivity: Activity = PrecedingNode(from)
    var TaskList: List[Activity] = List.empty

    while (CurrentActivity != null) {
      TaskList = CurrentActivity :: TaskList
      CurrentActivity = PrecedingNode(CurrentActivity)
    }

    TaskList
  }

  /**
   * returns the non-solid fragments of a critical path.
   *
   * @param p the planning including all tasks
   * @return a list of pair of activities. They are a subset of a critical path
   *         such that there is a tight additional dependency between them
   */
  def nonSolidCriticalPath(p: Planning)(from: Activity = p.sentinelActivity): List[(Activity, Activity)] = {

    def PrecedingNode(j: Activity): Activity = {
      if (j.definingPredecessors.value.isEmpty) null
      else p.activityArray(selectFrom(j.definingPredecessors.value))
      //random tie break, as it is likely that there will be few forks.
    }

    var CurrentActivity: Activity = if (from == p.sentinelActivity) PrecedingNode(from) else from
    var toreturn: List[(Activity, Activity)] = List.empty
    while (CurrentActivity != null) {
      val Predecessor = PrecedingNode(CurrentActivity)
      if (Predecessor != null && CurrentActivity.additionalPredecessors.value.contains(Predecessor.ID)) {
        toreturn = (Predecessor, CurrentActivity) :: toreturn
      }
      CurrentActivity = Predecessor
    }
    toreturn
  }

}
