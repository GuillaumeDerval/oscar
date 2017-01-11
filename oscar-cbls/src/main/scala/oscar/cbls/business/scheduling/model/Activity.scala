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

import oscar.cbls.core.computation.CBLSIntVar._
import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.minmax.{ArgMax, Max, MinArray}
import oscar.cbls.lib.invariant.set.{Inter, Union}
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.SortedSet

object Activity {
  def apply(duration: IntValue, planning: Planning, name: String = "",
            shifter: (IntValue, IntValue) => IntValue = (a: IntValue, _) => a) =
    new Activity(duration, planning, name, shifter)

  implicit val ord: Ordering[Activity] = new Ordering[Activity] {
    def compare(o1: Activity, o2: Activity) = o1.ID - o2.ID
  }
}
/**
 *
 * @param duration the duration of the activity
 * @param planning the planning in whih the activity takes place
 * @param name the name of the activity
 * @param shifter a function that builds a shifter. A shifter is a function: start,duration => shifted start, that postpones a starting date to avoid some impossibilities
 * @author renaud.delandtsheer@cetic.be
 */
class Activity(var duration: IntValue, val planning: Planning, val name: String = "",
               shifter: (IntValue, IntValue) => IntValue = (a: IntValue, _) => a) {
  val ID: Int = planning.addActivity(this)


  duration.domain.restrict(PositiveOrNullRange)
  val isTakenInSentinel = true

  override def equals(obj: Any): Boolean = {
    obj match {
      case a: Activity => a.ID == ID
      case _ => false
    }
  }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Activity]

  /**Used for marking algorithm. Must always be set to false between algorithm execution*/
  var mark: Boolean = false

  override def toString: String = name

  var staticPredecessors: List[Activity] = List.empty

  def addStaticPredecessor(j: Activity) {
    staticPredecessors = j :: staticPredecessors
    j.hasSuccessor = true
  }

  def precedes(j: Activity) {
    j.addStaticPredecessor(this)
  }

  var hasSuccessor:Boolean = false

  def uses(n: IntValue): ActivityAndAmount = ActivityAndAmount(this, n)

  case class ActivityAndAmount(t: Activity, amount: IntValue) {
    def ofResource(r: CumulativeResource) {
      t.usesCumulativeResource(r, amount)
    }

    def ofResources(rr: CumulativeResource*) {
      for (r <- rr) { t.usesCumulativeResource(r, amount) }
    }

    def ofResources(rr: Iterable[CumulativeResource]) {
      rr.foreach(t.usesCumulativeResource(_, amount))
    }
  }

  def removeNonTightAdditionalPredecessors() {
    for (iD: Int <- additionalPredecessors.value) {
      if (!potentiallyKilledPredecessors.value.contains(iD)) {
        additionalPredecessors :-= iD
      }
    }
  }

  var Resources: List[(CumulativeResource, IntValue)] = List.empty

  /**
   * use this method to add resource requirement to a activity.
   * the activity and the resource must be registered to the same planning
   * @param r a resource that the activity uses
   * @param amount the amount of this resource that the activity uses
   * FIXME potential problem if amount = 0
   */
  def usesCumulativeResource(r: CumulativeResource, amount: IntValue) {
    Resources = (r, amount) :: Resources
    r.notifyUsedBy(this, amount)
  }

  def maxDuration = planning.maxDuration
  val durationMinusOne = duration - 1

  val earliestStartDate: CBLSIntVar = CBLSIntVar(planning.model, 0, 0 to maxDuration,
    "esd(" + name + ")")
  val earliestEndDate: CBLSIntVar = CBLSIntVar(planning.model,
    duration.value, -1 to maxDuration, //-1 is used because activity end at "start + duration -1", and duration might be zero
    "eed(" + name + ")")
  earliestEndDate <== earliestStartDate + durationMinusOne

  val latestEndDate: CBLSIntVar = CBLSIntVar(planning.model, maxDuration, 0 to (maxDuration + 2), //idem, we have to do +1 here for the same reason
    "led(" + name + ")")

  var staticPredecessorsID:CBLSSetConst = null

  val latestStartDate = latestEndDate - durationMinusOne
  var allSucceedingActivities: CBLSSetVar = null

  var additionalPredecessors = new CBLSSetVar(planning.model, SortedSet.empty, 0 to planning.activities.size,
    "added predecessors of " + name)
  var allPrecedingActivities: SetValue = null

  var definingPredecessors: SetValue = null
  var potentiallyKilledPredecessors: SetValue = null

  def addDynamicPredecessor(t: Activity, verbose: Boolean = false) {
    if (verbose) println("added " + t + "->" + this)
    additionalPredecessors :+= t.getEndActivity.ID
  }

  def removeDynamicPredecessor(t: Activity, verbose: Boolean = false) {
    if (verbose) println("killed " + t + "->" + this)
    additionalPredecessors :-= t.getEndActivity.ID
  }

  def getEndActivity: Activity = this
  def getStartActivity: Activity = this

  def canAddPrecedence: Boolean = true

  // var ParasiticPrecedences:IntSetVar = null
  /**This method is called by the planning when all activities are created*/
  def close() {
    if (staticPredecessorsID == null) {

      staticPredecessorsID = CBLSSetConst(SortedSet.empty[Int] ++ staticPredecessors.map((j: Activity) => j.ID))

      allPrecedingActivities = Union(staticPredecessorsID, additionalPredecessors)

      earliestStartDate <== shifter(Max(planning.earliestEndDates, allPrecedingActivities, -1) + 1, duration)

      definingPredecessors = ArgMax(planning.earliestEndDates, allPrecedingActivities, -1)

      potentiallyKilledPredecessors = Inter(definingPredecessors, additionalPredecessors)

      allSucceedingActivities = new CBLSSetVar(planning.model, SortedSet.empty, 0 to planning.activityCount - 1,
        "succeeding_activities_of_" + name)

      latestEndDate <== MinArray(planning.latestStartDates, allSucceedingActivities,
          planning.maxDuration)-1
    }
  }

  def toAsciiArt: String = {
    def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

    padToLength(this.name, 20) + ":" +
      "[" + padToLength("" + this.earliestStartDate.value, 4) + ";" +
      padToLength("" + this.earliestEndDate.value, 4) + "] " +
      (if (this.duration.value == 1)
        nStrings(this.earliestStartDate.value, " ") + "#\n"
      else
        nStrings(this.earliestStartDate.value, " ") + "#" + nStrings(this.duration.value - 2, "=") + "#\n")
  }
}
