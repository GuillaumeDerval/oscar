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
package oscar.cbls.business.scheduling.model

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 * ****************************************************************************
 */

import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.logic.{DenseRef, Filter}
import oscar.cbls.lib.invariant.minmax.ArgMax
import oscar.cbls.lib.invariant.numeric.Sum

/**
 * @param model
 * @param maxDuration is the full duration considered here. The engine will crash if it needs to put an activity after this date
 * @author renaud.delandtsheer@cetic.be
 */
class Planning(val model: Store, val maxDuration: Int) {

  var isClosed = false

  var resources: List[Resource] = List.empty
  var resourceCount: Int = 0
  /**called by resources registers it in the planning, returns an ID, which is the one of the resource*/
  def addResource(r: Resource): Int = {
    resources = r :: resources
    resourceCount += 1
    resourceCount - 1
  }

  var superActivity = false
  def isThereAnySuperActitity = superActivity
  var activities: List[Activity] = List.empty
  var activityCount: Int = 0
  /**called by activities registers it in the planning, returns an ID, which is the one of the activity*/
  def addActivity(j: Activity): Int = {
    activities = j :: activities
    activityCount += 1
    activityCount - 1
  }

  var earliestStartDates: Array[CBLSIntVar] = null
  var earliestEndDates: Array[CBLSIntVar] = null
  var latestStartDates: Array[IntValue] = null

  val makeSpan = CBLSIntVar(model, 0, 0 to maxDuration, "makeSpan")
  var earliestOvershotResources: CBLSSetVar = null
  var worseOvershotResource: SetValue = null

  var resourceArray: Array[Resource] = null
  var activityArray: Array[Activity] = null

  var sentinelActivity: Activity = null //a task that is added after all activities, to simplify algorithm construction

  model.addToCallBeforeClose(() => this.close())

  /**
   * this is to close the planning when you are done with declaring tasks, precedence  and resource
   * notice that you do not need to explicitly call this, as the model will call it automatically on close.
   */
  def close() {
    if (isClosed) return
    isClosed = true
    val activitiesNoSentinel = activities
    sentinelActivity = new Activity(0, this, "sentinelActivity")
    sentinelActivity.latestEndDate := maxDuration

    for (a <- activitiesNoSentinel) {
      if (a.isTakenInSentinel && ! a.hasSuccessor){
        sentinelActivity.addStaticPredecessor(a)
      }
    }

    activityArray = new Array[Activity](activityCount)
    earliestEndDates = new Array[CBLSIntVar](activityCount)
    earliestStartDates = new Array[CBLSIntVar](activityCount)
    latestStartDates = new Array[IntValue](activityCount)

    for (j <- activities) {
      activityArray(j.ID) = j
      earliestStartDates(j.ID) = j.earliestStartDate
      earliestEndDates(j.ID) = j.earliestEndDate
      latestStartDates(j.ID) = j.latestStartDate
      if (j.isInstanceOf[SuperActivity]) superActivity = true
    }

    for (j <- activities) { j.close() }

    DenseRef(activityArray.map(job => job.allPrecedingActivities), activityArray.map(job => job.allSucceedingActivities))

    makeSpan <== sentinelActivity.earliestStartDate

    resourceArray = new Array[Resource](resourceCount)
    for (r <- resources) {
      resourceArray(r.ResourceID) = r; r.close()
    }

    val overshootArray: Array[IntValue] = new Array[IntValue](resourceCount)
    for (r <- resources) {
      overshootArray(r.ResourceID) = r.overShoot
    }

    val ResourceWithOvershoot: SetValue = Filter(overshootArray)

    worseOvershotResource = ArgMax(overshootArray, ResourceWithOvershoot)
  }

  override def toString: String = toAsciiArt

  def toAsciiArt: String = {
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    val activityList =
      activities.filter(_ != sentinelActivity).sortWith((a, b) =>
        a.earliestStartDate.value < b.earliestStartDate.value)
    val activityStrings = activityList.map(_.toAsciiArt)

    activityStrings.mkString + makeSpan + "\n"
  }

  def dependencies: String = {
    var toreturnAdditional:String = ""
    var toreturnInitial:String = ""
    for (activity <- activities.sortBy(t => t.earliestStartDate.value)) {
      for (t2 <- activity.allSucceedingActivities.value if t2 != activity.ID && t2 != sentinelActivity.ID) {
        val activity2 = activityArray(t2)
        if (activity2.additionalPredecessors.value.contains(activity.ID)) {
          toreturnAdditional += " " + activity.name + " -> " + activity2.name + "\n"
        } else {
          toreturnInitial += " " + activity.name + " -> " + activity2.name + "\n"
        }
      }
    }
    "initial dependencies:\n" + toreturnAdditional +
    "added dependencies:\n" + toreturnInitial
  }

  def resourceUsage: String = {
    resources.map(_.toAsciiArt(20)).mkString
  }
  /**
   * Checks that a dependence from --> to can be added to the graph,
   * assuming that there is a resource conflict involving them
   * @param from
   * @param to
   * @return true if a dependence can be add, false otherwise.
   */
  def canAddPrecedenceAssumingResourceConflict(from: Activity, to: Activity): Boolean = {
    //this is not straightforward since there can be some SuperTasks.
    (from != to) && to.canAddPrecedence && ((!superActivity) || !isThereDependency(to, from))
  }

  /**
   * Checks if there is a path leading from one activity to another one
   * @param from
   * @param to
   * @return true if there is a path from 'from' to 'to', false otherwise
   */
  def isThereDependency(from: Activity, to: Activity): Boolean = {
    val target = to.getEndActivity

    var Reached: List[Activity] = List.empty

    /**PRE: from is a ground activity. */
    def Search(from: Activity): Boolean = {
      if (from == target) return true
      if (from.earliestEndDate.value > target.earliestStartDate.value) {
        return false
      }

      if (from.mark) { return false }
      from.mark = true
      Reached = from :: Reached
      for (next <- from.getStartActivity.allSucceedingActivities.value) {
        val activity: Activity = activityArray(next)
        if (Search(activity)) return true
      }
      false
    }

    val toreturn = Search(from.getStartActivity)
    for (activity <- Reached) activity.mark = false
    toreturn
  }

  /**
   * returns a list of pair of activity; precedences to kill
   * to add the new dependency newFrom -> newTo
   * without introducing a cycle involving newFrom -> newTo
   *
   * it computes a cut in the dag newTo -> newFrom involving only additional dependencies
   * @param newFrom
   * @param newTo
   * @return
   */
  def getDependencyToKillToAvoidCycle(newFrom: Activity, newTo: Activity): PrecedenceCleaner = {
    val from = newTo
    val to = newFrom
    if (from == to) return HardPrecedence()

    var markedActivities: List[Activity] = List.empty
    var dependenciesToKill: List[(Activity, Activity)] = List.empty
    /**
     * marks all activities on the path linking From to To
     * all market activities are also added to MarkedActivities
     * return true if a path exist
     */
    def markPathes(from: Activity, to: Activity): Boolean = {
      if (from.mark) return true
      //TODO: we might explore the same node several times if it does not lead to to
      if (from == to) {
        if (!from.mark) {
          from.mark = true
          markedActivities = from :: markedActivities
        }
        return true
      }
      if (from.earliestEndDate.value > to.earliestStartDate.value) {
        return false
      }

      for (next <- from.getStartActivity.allSucceedingActivities.value) {
        val nextActivity: Activity = activityArray(next)
        if (markPathes(nextActivity, to)) from.mark = true
      }
      if (from.mark) {
        markedActivities = from :: markedActivities
      }
      from.mark
    }

    /**returns false if hard rock dependency, true if can be killed*/
    def findDependenciesToKill(from: Activity, to: Activity): Boolean = {
      if (from == to) return false
      if (!to.mark) { return true }
      for (prev <- to.getStartActivity.additionalPredecessors.value) {
        val prevActivity: Activity = activityArray(prev)
        if (prevActivity.mark) {
          dependenciesToKill = (prevActivity, to) :: dependenciesToKill
          prevActivity.mark = false
        }
      }
      for (prevActivity <- to.getStartActivity.staticPredecessors) {
        if (prevActivity.mark) {
          if (findDependenciesToKill(from, prevActivity)) {
            prevActivity.mark = false
          } else {
            return false
          }
        }
      }
      true
    }

    markPathes(from.getStartActivity, to.getEndActivity)
    if (!findDependenciesToKill(from.getStartActivity, to.getEndActivity)) {
      for (t <- markedActivities) t.mark = false
      HardPrecedence()
    } else {
      for (t <- markedActivities) t.mark = false
      PrecedencesCanBeKilled(dependenciesToKill)
    }
  }

  /**
   * removes all additional Activity precedences that are not tight
   */
  def clean() {
    for (t: Activity <- activityArray) {
      t.removeNonTightAdditionalPredecessors()
    }
  }
}

abstract class PrecedenceCleaner(val canBeKilled: Boolean) {
  def killDependencies(Verbose: Boolean = false) { throw new Exception("cannot kill dependencies") }
}
case class HardPrecedence() extends PrecedenceCleaner(false)

case class PrecedencesCanBeKilled(val d: List[(Activity, Activity)]) extends PrecedenceCleaner(true) {
  override def killDependencies(Verbose: Boolean = false) {
    for ((a, b) <- d) {
      b.removeDynamicPredecessor(a, Verbose)
    }
  }
  def restoreDependencies() {
    for ((a, b) <- d) {
      b.addDynamicPredecessor(a)
    }
  }
}

/**
 * This trait lets one post an earliest start date for a given task.
 *
 * @author yoann.guyot@cetic.be
 */
trait EarliestStartDate extends Planning {
  /**
   * Technically, it creates a task which ends at the wanted earliest start date,
   * and posts it as a static predecessor of the given task.
   *
   * Note: It seems that using an Activity (instead of a NonMoveableActivity)
   * saves scheduling engine performances.
   */
  def addEarliestStartDate(task: Activity, earliestStartDate: Int,
                           nameOfOwner: String = "") {
    if (earliestStartDate > 0) {
      val esdTaskName = "ESD_task_of_" + nameOfOwner
      val esdTask = new Activity(earliestStartDate, this, esdTaskName)
      task.addStaticPredecessor(esdTask)
    }
  }
}

/**
 * This trait lets one use activities with due dates.
 *
 * @author yoann.guyot@cetic.be
 */
trait Deadlines extends Planning {
  var activitiesWithDeadlines: List[ActivityWithDeadline] = List.empty
  val totalTardiness = CBLSIntVar(model, 0, name = "Total tardiness")

  model.addToCallBeforeClose(() => closeDeadlines())

  private def closeDeadlines() {
    totalTardiness <== Sum(activitiesWithDeadlines.toArray.map(_.tardiness))
  }

  def addActivityWithDeadline(activity: ActivityWithDeadline) {
    activitiesWithDeadlines = activity :: activitiesWithDeadlines
  }
}

/**
 * This trait lets one post variable resources.
 * Typically: resources which vary with week days.
 *
 * Technically, variable resources are cumulative resources,
 * of which we restrict the availability (at time t)
 * using non-movable activities.
 * @author yoann.guyot@cetic.be
 */
trait VariableResources extends Planning {
  /**
   * This array contains the list of restricting activities,
   * indexed by the scheduler's unit.
   */
  val resourceRestrictionTasks: Array[List[NonMoveableActivity]] =
    Array.fill(maxDuration + 1)(List())

  /**
   * One should use this function to add a variable resource
   * to his scheduling model.
   *
   * Availabilities(t) = amount of the resource available at time t
   * If the size of availabilities is less than the horizon, it will be
   * used modulo this size:
   *    let t be such that: availabilities.size <= t <= horizon
   *    the amount of resource at time t will be:
   *    availabilities(t % availabilities.size)
   * @author yoann.guyot@cetic.be
   */
  def postVariableResource(availabilities: Array[Int],
                           name: String = null): VariableResource = {
    VariableResource(this, availabilities, name)
  }
}

/**
 * This trait maintains the total resources overshoot value of a planning.
 * @author yoann.guyot@cetic.be
 */
trait TotalResourcesOvershootEvaluation extends Planning {
  val totalOvershoot = CBLSIntVar(model, 0, name = "Total resources overshoot")
  
  model.addToCallBeforeClose(() => setTotalOvershoot())

  private def setTotalOvershoot() = {
    val overshootArray: Array[IntValue] = new Array[IntValue](resourceCount)
    for (r <- resources) {
      overshootArray(r.ResourceID) = r.overShoot
    }
    totalOvershoot <== Sum(overshootArray)
  }
}
