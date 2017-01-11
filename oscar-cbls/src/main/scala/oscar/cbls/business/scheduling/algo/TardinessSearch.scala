package oscar.cbls.business.scheduling.algo

import oscar.cbls.business.scheduling.algo.CriticalPathFinder.nonSolidCriticalPath
import oscar.cbls.business.scheduling.model.{Activity, Deadlines, Planning, TotalResourcesOvershootEvaluation, VariableResources}
import oscar.cbls.core.computation.{Solution, Store, Variable}
import oscar.cbls.lib.search.LinearSelectorTrait

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
 *         by Yoann Guyot
 * ****************************************************************************
 */

/**
 * @param planning
 * @param maxLocalIterations
 * @param temperature Metropolis parameter
 * @param verbose
 * @author yoann.guyot@cetic.be
 */
class TardinessSearch(planning: Planning with Deadlines with TotalResourcesOvershootEvaluation with VariableResources,
                      temperature: Float = 100,
                      verbose: Boolean = false) extends LinearSelectorTrait {
  val model: Store = planning.model

  require(model.isClosed, "model should be closed before TardinessSearch algo can be instantiated")

  var minOvershootValue: Int = 0
  var bestSolution: Solution = null

  /**
   * This solves the jobshop by neighborhood exploration. This is done by applying
   * movements in a set of critical activities.
   * @param maxTrials the max number of iterations of the search
   * @param stable the max number of successive iterations with no improvement
   */
  def solve(maxTrials: Int,
            stable: Int,
            maxLocalIterations: Int = 5,
            onlyImprovingMoves: Boolean = true,
            saveCurrentSolution: Boolean = false) = {
    var hasImproved = false
    if (planning.totalTardiness.value > 0) {
      if (saveCurrentSolution) {
        bestSolution = model.solution(true)
        minOvershootValue = planning.totalOvershoot.value
      }
      var nbTrials: Int = 0
      var continue: Boolean = true

      while (nbTrials < maxTrials && continue) {
        if (verbose) println("Tardiness search trial " + nbTrials + ".")
        continue = false

        var precCriticalActivities: List[(Activity, Activity)] = List()
        for (activity <- planning.activitiesWithDeadlines.filter(_.isLate)) {
          if (verbose) println("Activity " + activity.name + " is late.")
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
            if (verbose) println("Skip (nothing to do).\n")
          }
          precCriticalActivities = criticalActivities
        }
        nbTrials = nbTrials + 1
      }

      if (bestSolution != null) model.restoreSolution(bestSolution)
      planning.clean()
      if (verbose) println("Restored best solution.")
    }
    hasImproved
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

      if (verbose) println("Exploration iteration " + i + ".")
      moved = false
      val (from, to) = criticalsIterator.next

      if (verbose) println("tardiness: " + planning.totalTardiness)
      val (gain, beforeSwapSnapshot) = swap(from, to)
      if (verbose) println("tardiness: " + planning.totalTardiness)

      if (verbose) println("gain = " + gain)

      if (gain < 0) {
        val currentOvershoot = planning.totalOvershoot.value
        if (verbose) println("overshoot = " + minOvershootValue + " -> " + currentOvershoot)
        if (currentOvershoot <= minOvershootValue) {
          minOvershootValue = currentOvershoot
          bestSolution = model.solution(true)
          hasImproved = true
          if (verbose) println("(improvement) Swapped " + from + " with " + to + "\n")
          moved = true
        } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
          if (verbose) println("(random " + temperature + "°) Swapped "
            + from + " with " + to + "\n")
          moved = true
        } else {
          // cancel move
          planning.model.restoreSolution(beforeSwapSnapshot)
          if (verbose) println("No move (Swap undone).\n")
        }
        // metropolis criterion
      } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
        if (verbose) println("(random " + temperature + "°) Swapped "
          + from + " with " + to + "\n")
        moved = true
      } else {
        // cancel move
        planning.model.restoreSolution(beforeSwapSnapshot)
        if (verbose) println("No move (Swap undone).\n")
      }

      i = i + 1
    }

    hasImproved
  }

  private def swap(from: Activity, to: Activity): (Int, Solution) = {
    val successors = to.allSucceedingActivities.value.toList.map(planning.activityArray(_))
    val successorsPredecessors = successors.map(_.additionalPredecessors)
    val activitiesToSnap:List[Variable] = from.additionalPredecessors :: to.additionalPredecessors :: successorsPredecessors
    val snapshot = planning.model.saveValues(activitiesToSnap)

    val previousTardiness = planning.totalTardiness.value
    val gain = if (to.canAddPrecedence) {
      to.removeDynamicPredecessor(from, verbose)

      val predecessors = from.allPrecedingActivities.value.toList.map(planning.activityArray(_))
      predecessors.foreach(
        (pred: Activity) =>
          if (!from.staticPredecessors.contains(pred))
            to.addDynamicPredecessor(pred, verbose))

      val successors = to.allSucceedingActivities.value.toList.map(planning.activityArray(_))
      successors.foreach(
        (succ: Activity) =>
          if (succ.canAddPrecedence) {
            if (!succ.staticPredecessors.contains(to))
              succ.addDynamicPredecessor(from, verbose)
          })

      if (from.canAddPrecedence)
        from.addDynamicPredecessor(to, verbose)

      val currentTardiness = planning.totalTardiness.value
      currentTardiness - previousTardiness
    } else {
      -1
    }

    (gain, snapshot)
  }
}
