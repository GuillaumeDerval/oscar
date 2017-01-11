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

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.business.scheduling.model

import oscar.cbls.core.computation.{CBLSSetVar, IntValue}
import oscar.cbls.lib.invariant.minmax.MinArray

import scala.collection.immutable.SortedSet

/**
 * @param startDate
 * @param duration
 * @param planning
 * @param name
 * @author renaud.delandtsheer@cetic.be
 * THIS IS EXPERIMENTAL
 */
class NonMoveableActivity(startDate: Int, duration: IntValue, planning: Planning,
                          name: String = "")
  extends Activity(duration, planning, name) {

  override val isTakenInSentinel = false

  if (startDate + duration.min > maxDuration)
    sys.error("Cannot post non moveable activity " + name +
      " because it exceeds the scheduler horizon (startDate + duration.minVal = " +
      (startDate + duration.min) + ").")

  override def canAddPrecedence: Boolean = false
  override def close() {

    additionalPredecessors := SortedSet.empty[Int]
    allPrecedingActivities = SortedSet.empty[Int]
    earliestStartDate := startDate
    definingPredecessors = SortedSet.empty[Int]
    potentiallyKilledPredecessors = SortedSet.empty[Int]

    allSucceedingActivities = new CBLSSetVar(planning.model, SortedSet.empty, 0 to planning.activityCount - 1,
      "succeeding_activities_of_" + name)

    //This is not correct. but since no task can be put before this one, this is not an issue.
    latestEndDate <== MinArray(planning.latestStartDates, allSucceedingActivities,
      planning.maxDuration)
  }

  override def addStaticPredecessor(j: Activity): Unit = {
    throw new Exception("NonMoveableActivity cannot have a static predecessor activity. ")
  }
}

object NonMoveableActivity {
  def apply(startDate: Int, duration: IntValue, planning: Planning, name: String = "") =
    new NonMoveableActivity(startDate, duration, planning, name)
}