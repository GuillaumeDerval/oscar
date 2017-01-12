package oscar.cbls.business.scheduling.solver

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


/*


/**
 * Created by rdl on 5/09/2014.
 */
case class Swap(planning:Planning with Deadlines with TotalResourcesOvershootEvaluation,objective:CBLSIntVar, best:Boolean)
  extends EasyNeighborhood(best,objective) {

  val model:Store = planning.model

  /** This is the method you ust implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    var precCriticalActivities: List[(Activity, Activity)] = List()
    for (activity <- planning.activitiesWithDeadlines.filter(_.isLate)) {
      if (amIVerbose) println("Activity " + activity.name + " is late.")
      // a list of (predecessor, activity) with an additional tight dependence
      val criticalActivities = nonSolidCriticalPath(planning)(activity)
      // if (exploreNeighborhood(criticalActivities, maxLocalIterations))
      if (!criticalActivities.isEmpty
        && criticalActivities != precCriticalActivities) {
        if (exploreNeighborhood(criticalActivities,
          maxLocalIterations, onlyImprovingMoves)) {
          hasImproved = true
          continue = true
        }
      } else {
        if (amIVerbose) println("Skip (nothing to do).\n")
      }
      precCriticalActivities = criticalActivities
    }
  }

  // a list of (predecessor, activity) with an additional tight dependence
  private def exploreNeighborhood(criticals: List[(Activity, Activity)],
                                  maxLocalIterations: Int,
                                  onlyImproving: Boolean = true) = {
    var hasImproved = false
    var moved = false
    var i = 0

    var criticalsIterator = criticals.iterator

    while (!moved
      && (if (maxLocalIterations > 0) i < maxLocalIterations else true)
      && criticalsIterator.hasNext) {

      if (amIVerbose) println("Exploration iteration " + i + ".")
      moved = false
      val (from, to) = criticalsIterator.next

      if (amIVerbose) println("tardiness: " + planning.totalTardiness)
      val (gain, beforeSwapSnapshot) = swap(from, to)
      if (amIVerbose) println("tardiness: " + planning.totalTardiness)

      if (amIVerbose) println("gain = " + gain)

      if (gain < 0) {
        val currentOvershoot = planning.totalOvershoot.value
        if (amIVerbose) println("overshoot = " + minOvershootValue + " -> " + currentOvershoot)
        if (currentOvershoot <= minOvershootValue) {
          minOvershootValue = currentOvershoot
          bestSolution = model.solution(true)
          hasImproved = true
          if (amIVerbose) println("(improvement) Swapped " + from + " with " + to + "\n")
          moved = true
        } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
          if (amIVerbose) println("(random " + temperature + "°) Swapped "
            + from + " with " + to + "\n")
          moved = true
        } else {
          // cancel move
          planning.model.restoreSnapshot(beforeSwapSnapshot)
          if (amIVerbose) println("No move (Swap undone).\n")
        }
        // metropolis criterion
      } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
        if (amIVerbose) println("(random " + temperature + "°) Swapped "
          + from + " with " + to + "\n")
        moved = true
      } else {
        // cancel move
        planning.model.restoreSnapshot(beforeSwapSnapshot)
        if (amIVerbose) println("No move (Swap undone).\n")
      }

      i = i + 1
    }

    hasImproved
  }

  private def swap(from: Activity, to: Activity): (Int, Snapshot) = {
    val successors = to.allSucceedingActivities.value.toList.map(planning.activityArray(_))
    val successorsPredecessors = successors.map(_.additionalPredecessors)
    val activitiesToSnap = from.additionalPredecessors :: to.additionalPredecessors :: successorsPredecessors
    val snapshot = planning.model.saveValues(activitiesToSnap: _*)

    val previousTardiness = planning.totalTardiness.value
    val gain = if (to.canAddPrecedence) {
      to.removeDynamicPredecessor(from, amIVerbose)

      val predecessors = from.allPrecedingActivities.value.toList.map(planning.activityArray(_))
      predecessors.foreach(
        (pred: Activity) =>
          if (!from.staticPredecessors.contains(pred))
            to.addDynamicPredecessor(pred, amIVerbose))

      val successors = to.allSucceedingActivities.value.toList.map(planning.activityArray(_))
      successors.foreach(
        (succ: Activity) =>
          if (succ.canAddPrecedence) {
            if (!succ.staticPredecessors.contains(to))
              succ.addDynamicPredecessor(from, amIVerbose)
          })

      if (from.canAddPrecedence)
        from.addDynamicPredecessor(to, amIVerbose)

      val currentTardiness = planning.totalTardiness.value
      currentTardiness - previousTardiness
    } else {
      -1
    }

    (gain, snapshot)
  }
}

*/