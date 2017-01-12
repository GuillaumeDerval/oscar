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

import oscar.cbls.business.scheduling.algo.CriticalPathFinder
import oscar.cbls.business.scheduling.model.{Activity, NonMoveableActivity, Planning, PrecedenceCleaner, Resource}
import oscar.cbls.core.computation.IntValue.toFunction
import oscar.cbls.core.computation.SetValue.toFunction
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.objective.Objective.objToFun
import oscar.cbls.core.search.{JumpNeighborhood, JumpNeighborhoodParam, Neighborhood}
import oscar.cbls.lib.search.LinearSelectorTrait
import oscar.cbls.lib.search.combinators.BasicSaveBest

import scala.language.postfixOps

/**
 * @param p the planning to flatten
 * @param maxIterations the max number of the flattening.
 *                      This is a safety parameter to prevent infinite loop,
 *                      you can set it to (p.activityCount * (p.activityCount - 1)) / 2
 * @param estimateMakespanExpansionForNewDependency  This computes an estimate of the MakeSpan expansion if the given precedence is added.
 *                                                   this estimate is completely wrong in itself, as a constant factor is added to each estimate.
 *                                                   since it is the same factor, you can use this method to chose among a set of precedence
 *                                                   because this will forget about the correcting factor.
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 */
case class FlattenWorseFirst(p: Planning,
                             maxIterations: Int,
                             estimateMakespanExpansionForNewDependency: (Activity, Activity) => Int = (from: Activity, to: Activity) => from.earliestEndDate.value - to.latestStartDate.value,
                             priorityToPrecedenceToMovableActivities: Boolean = true)(supportForSuperActivities: Boolean = p.isThereAnySuperActitity)
  extends JumpNeighborhood with LinearSelectorTrait {

  require(p.isClosed)
  override def shortDescription(): String = "Flattening worse first"

  //this resets the internal state of the Neighborhood
  override def reset() {}

  /** implements the standard flatten procedure */
  override def doIt() {
    var iterations = 0
    while (p.worseOvershotResource.value.nonEmpty) {
      if (iterations > maxIterations)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. " +
          "Check there is no conflict between non movable activities.")
      iterations += 1

      // the most violated resource
      val r: Resource = p.resourceArray(selectFrom(p.worseOvershotResource.value))

      // the worse violation of the resource in time
      val t: Int = r.worseOverShootTime

      if (!flattenOne(r, t)) {

        if (!supportForSuperActivities)
          throw new Error("cannot flatten until conflict resolution, maybe your model has superActivities?" +
            " if yes set supportForSuperActivities. ")

        flattenOneWithSuperTaskHandling(r, t)
      }
    }
    //    print(p.toAsciiArt)
  }

  /**
   * @param r
   * @param t
   * @return true if flattening was performed, false otherwise.
   */
  def flattenOne(r: Resource, t: Int): Boolean = {
    val conflictActivities = r.conflictingActivities(t)
    val baseForEjection = r.baseActivityForEjection(t)

    val makeSpanExpansionEstimator = (if (priorityToPrecedenceToMovableActivities)
      (from: Activity, to: Activity) =>
      2 * estimateMakespanExpansionForNewDependency(from, to)
        + (if (from.isInstanceOf[NonMoveableActivity]) 1 else 0)
    else estimateMakespanExpansionForNewDependency)

    selectMin2[Activity, Activity](baseForEjection, conflictActivities,
      makeSpanExpansionEstimator,
      p.canAddPrecedenceAssumingResourceConflict) match {
        case (a, b) =>
          b.addDynamicPredecessor(a, printTakenMoves)
          return true
        case null => return false;
      }
  }

  def flattenOneWithSuperTaskHandling(r: Resource, t: Int): Unit = {
    val conflictActivities = r.conflictingActivities(t)
    val baseForEjection = r.baseActivityForEjection(t)

    //no precedence can be added because some additional precedence must be killed to allow that
    //this happens when superTasks are used, and when dependencies have been added around the start and end tasks of a superTask
    //we search which dependency can be killed in the conflict set,
    val conflictActivityArray = conflictActivities.toArray
    val baseForEjectionArray = baseForEjection.toArray

    val dependencyKillers: Array[Array[PrecedenceCleaner]] =
      Array.tabulate(baseForEjection.size)(
        t1 => Array.tabulate(conflictActivityArray.size)(
          t2 => p.getDependencyToKillToAvoidCycle(baseForEjectionArray(t1), conflictActivityArray(t2))))

    selectMin2(baseForEjectionArray.indices, conflictActivityArray.indices,
      (a: Int, b: Int) => estimateMakespanExpansionForNewDependency(baseForEjectionArray(a), conflictActivityArray(b)),
      (a: Int, b: Int) => dependencyKillers(a)(b).canBeKilled) match {
        case (a, b) =>
          if (printTakenMoves) println("need to kill dependencies to complete flattening")
          dependencyKillers(a)(b).killDependencies(printTakenMoves)

          conflictActivityArray(b).addDynamicPredecessor(baseForEjectionArray(a), printTakenMoves)

        case null => throw new Error("cannot flatten at time " + t + " activities: " + conflictActivities)
      }
  }
}

/**
 * @param p the planning to relax
 * @param pKill the probability to kill a killable precedence constraint in percent. must be bigger than 10 (otherwise this will crash
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 */
case class Relax(p: Planning, pKill: Int,
                 doRelax: (Activity, Activity, Boolean) => Unit = (from: Activity, to: Activity, verbose: Boolean) => to.removeDynamicPredecessor(from, verbose))(activitiesToRelax: () => Iterable[Int] = p.sentinelActivity.staticPredecessorsID)
  extends JumpNeighborhoodParam[List[(Activity, Activity)]] with LinearSelectorTrait {

  override def doIt(potentiallyKilledPrecedences: List[(Activity, Activity)]) {
    for ((from, to) <- potentiallyKilledPrecedences) {
      doRelax(from, to, printTakenMoves)
    }
  }

  override def getParam: List[(Activity, Activity)] = {

    val activityToRelax = selectMax(activitiesToRelax(), (activityID: Int) => p.activityArray(activityID).earliestEndDate.value)
    val potentiallyKilledPrecedences = CriticalPathFinder.nonSolidCriticalPath(p)(p.activityArray(activityToRelax))
    if (potentiallyKilledPrecedences.isEmpty) null
    else {
      var toReturn: List[(Activity, Activity)] = List.empty
      var maxTrials = 0
      while (toReturn.isEmpty && maxTrials < 10) {
        maxTrials += 1
        toReturn = potentiallyKilledPrecedences.filter(_ => flip(pKill))
      }
      if (toReturn.isEmpty) potentiallyKilledPrecedences
      else toReturn
    }
  }

  override def getShortDescription(param: List[(Activity, Activity)]): String =
    "Relax critical Path " + param.map { case (a, b) => a + "->" + b }.mkString(", ")

  //this resets the internal state of the Neighborhood
  override def reset() {}
}

/**
 * relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource
 * Warning: can only be called if there are no existing conflict!!
 * @param p the planning to relax
 * @param twoPhaseCheck set to true for a possibly faster move evaluation,
 *                      but this depends on your model and propagation setup,
 *                      so you need to experiment on this option.
 *                      it has no influence on the result, only on the speed of this neighborhood.
 */
case class RelaxNoConflict(p: Planning, twoPhaseCheck: Boolean = false)
  extends JumpNeighborhood with LinearSelectorTrait {

  override def doIt(): Unit = {
    require(p.worseOvershotResource.value.isEmpty)

    var relaxCount = 0
    var improved = true
    while (improved) {
      improved = false

      for (t: Activity <- p.activityArray) {
        for (iD: Int <- t.additionalPredecessors.value) {

          val testedPredecessor = p.activityArray(iD)
          val dependencyCanBeKilledWithoutMoreCheck =
            if (twoPhaseCheck) !t.potentiallyKilledPredecessors.value.contains(iD) else false

          t.removeDynamicPredecessor(testedPredecessor, false)
          if (dependencyCanBeKilledWithoutMoreCheck || p.worseOvershotResource.value.isEmpty) {
            relaxCount += 1
            improved = true
          } else {
            t.addDynamicPredecessor(testedPredecessor, false)
          }
        }
      }
    }
    if (printTakenMoves) println("RelaxNoConflict: relaxCount:" + relaxCount)
  }
  override def shortDescription(): String = "relaxes all precedences without introducing a conflict (based on planning.worseOvershotResource)"
}

/**
 * removes all additional Activity precedences that are not tight
 * @param p the planning to relax
 * THIS IS COMPLETELY NEW EXPERIMENTAL AND UNTESTED
 */
case class CleanPrecedences(p: Planning) extends JumpNeighborhood with LinearSelectorTrait {

  override def doIt() {
    for (t: Activity <- p.activityArray) {
      for (iD: Int <- t.additionalPredecessors.value) {
        if (!t.potentiallyKilledPredecessors.value.contains(iD)) {
          t.removeDynamicPredecessor(p.activityArray(iD), printTakenMoves)
        }
      }
    }
  }

  override def shortDescription(): String =
    "removes all additional Activity precedences that are not tight"
}

object SchedulingStrategies {

  /**
   * @param p the planning
   * @param pKillPerRelax the probability of killing a precedence for each precedence on the critical path considered during a relax
   * @param stable the number of no successive no improve that will cause the search to stop
   * @param objective: the objective, typically the makespan, but you could try something else
   * @return a neighborhood, you just have to do all moves, and restore the best solution
   */
  def iFlatRelax(p: Planning,
                 objective: Objective,
                 nbRelax: Int = 4,
                 pKillPerRelax: Int = 50,
                 stable: Int,
                 displayPlanning: Boolean = false): Neighborhood = {
    require(p.model.isClosed, "model should be closed before iFlatRelax algo can be instantiated")
    val maxIterationsForFlatten = (p.activityCount * (p.activityCount - 1)) / 2

    val flatten = FlattenWorseFirst(p, maxIterationsForFlatten)() afterMove {
      if (displayPlanning) println(p.toAsciiArt)
      println(objective)
    }
    val relax = Relax(p, pKillPerRelax)()

    //search Loop is a round Robin
    val searchLoop = flatten step relax repeat nbRelax

    (searchLoop maxMoves stable * 4 withoutImprovementOver objective
      saveBest objective whenEmpty p.worseOvershotResource restoreBestOnExhaust) exhaust (CleanPrecedences(p) once)
  }

  def iFlatRelaxUntilMakeSpanReduced(p: Planning,
                                     nbRelax: Int = 4,
                                     pKillPerRelax: Int = 50,
                                     stable: Int,
                                     objective: Objective,
                                     displayPlanning: Boolean = false): BasicSaveBest = {
    require(p.model.isClosed, "model should be closed before iFlatRelax algo can be instantiated")
    val maxIterationsForFlatten = (p.activityCount * (p.activityCount - 1)) / 2

    val flatten = FlattenWorseFirst(p, maxIterationsForFlatten)() afterMove {
      if (displayPlanning) println(p.toAsciiArt)
      println(objective)
    }
    val relax = Relax(p, pKillPerRelax)()

    val searchStep = (relax untilImprovement (p.makeSpan, nbRelax, maxIterationsForFlatten)) exhaust (flatten maxMoves 1)

    (flatten sequence (searchStep atomic ()) maxMoves stable withoutImprovementOver objective
      saveBest objective whenEmpty p.worseOvershotResource)
  }

  //  val searchLoop = FlattenWorseFirst(p,maxIterationsForFlatten) maxMoves 1 afterMove {if (displayPlanning) println(p.toAsciiArt)} exhaustBack
  //    Relax(p, pKillPerRelax) untilImprovement(p.makeSpan, nbRelax, maxIterationsForFlatten)

  /*
      //TODO: moves should have reference to their originating neighborhoods
      //TODO instrumented moves should just inherit from the original move
      val flatten = FlattenWorseFirst(p,maxIterationsForFlatten) name "flatten" afterMove {if (displayPlanning) println(p.toAsciiArt)}
      val relaxes = Relax(p, pKillPerRelax) untilImprovement(p.makeSpan, nbRelax, maxIterationsForFlatten)

      val searchLoop2 = flatten sequence (relaxes orElse (flatten maxMoves stable suchThat (_.neighborhoodName.equals("flatten")) withoutImprovementOver objective) exhaust (flatten maxMoves 1)

      searchLoop2 protectBest objective whenEmpty p.worseOvershotResource restoreBestOnExhaust()
      */
}
