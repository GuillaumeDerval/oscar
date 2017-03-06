package oscar.cbls.scheduling.solver

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.objective.{IntVarObjective, Objective}
import oscar.cbls.scheduling.model._
import oscar.cbls.search.move.Move
import oscar.cbls.search.{SearchEngine, SearchEngineTrait}
import oscar.cbls.search.core.EasyNeighborhood


case class FlattenOne(p: Planning,
                      resourcesForFlattening:Planning=> Iterable[Resource] = (p:Planning) => List(p.resourceArray(SearchEngine.selectFrom(p.worseOvershotResource.value))),
                      timesForFlattening:Resource => Iterable[Int] = (r:Resource) => List(r.worseOverShootTime),
                      candidatesForDependency:(Resource,Int) => Iterable[Activity] = (r:Resource,t:Int) => r.conflictingActivities(t),
                      best:Boolean = false,
                      neighborhoodName:String=null,
                      priorityToPrecedenceToMovableActivities: Boolean = true)
  extends EasySchedulingNeighborhood[AddDynamicPrecedence](best,neighborhoodName) with SearchEngineTrait {
  require(p.isClosed)

  //this resets the internal state of the Neighborhood
  override def reset() {}

  var fromActivityForInstantiation:Activity = null
  var toActivityForInstantiation:Activity = null

  /** This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {

    for (resourceForFlattening <- resourcesForFlattening(p)) {
      for (timeForFlattening <- timesForFlattening(resourceForFlattening)) {
        val conflictActivities = candidatesForDependency(resourceForFlattening,timeForFlattening)

        for (fromActivity <- conflictActivities){
          for (toActivity <- conflictActivities if toActivity != fromActivity){
            fromActivityForInstantiation = fromActivity
            toActivityForInstantiation = toActivity
            if (evaluateCurrentMoveObjTrueIfStopRequired(obj.addDPrecedenceVal(fromActivity,toActivity))) {
              return
            }
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj : Int) : AddDynamicPrecedence = {
    AddDynamicPrecedence(fromActivityForInstantiation,toActivityForInstantiation)
  }
}

case class AddDynamicPrecedence(from:Activity,to:Activity) extends Move{
  /** to actually take the move */
  override def commit(): Unit = from.addDynamicPredecessor(to)
}

/*
case class FlattenOneSuperActivity(p: Planning,
                                   priorityToPrecedenceToMovableActivities: Boolean = true)
  extends EasyNeighborhood with SearchEngineTrait {

  val supportForSuperActivities: Boolean = p.isThereAnySuperActitity

  def flattenOneWithSuperTaskHandling(r: Resource, t: Int): Unit = {
    val conflictActivities = r.conflictingActivities(t)
    val baseForEjection = r.activitieUsingResourceAtThisTime(t)

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
*/