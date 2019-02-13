package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.algo.seq.IntSequenceExplorer
import oscar.cbls.business.seqScheduling.model.SchedulingProblem
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class RemoveActivity(schP: SchedulingProblem,
                     neighborhoodName: String = "RemoveActivity",
                     selectActivityBehavior:LoopBehavior = First(),
                     activitiesToRemove: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[RemoveActivityMove](neighborhoodName) {

  var currentExplorer:IntSequenceExplorer = null
  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Int): Unit = {
    // Iteration zone on activities indices
    // Checking the Hot Restart
    val iterationZone1:Iterable[IntSequenceExplorer] = activitiesToRemove match {
      case Some(activities:Iterable[Int]) => activities.flatMap(a => schP.activitiesPriorList.value.explorerAtAnyOccurrence(a))
      case None => schP.activitiesPriorList.value.explorerAtPosition(0).get.forward
    }

    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schP.activitiesPriorList.defineCurrentValueAsCheckpoint(true)
    // iterating over the indices in the activity list
    val (activityExplIterator, notifyFound) = selectActivityBehavior.toIterator(iterationZone1)

    while (activityExplIterator.hasNext) {
      currentExplorer = activityExplIterator.next()

      performRemove()
      val newObj = obj.value
      // Notification of finding indices
      if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
        notifyFound()
      }
      // Rollback to checkpoint
      schP.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
    }

    currentExplorer = null
    schP.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): RemoveActivityMove =
    RemoveActivityMove(schP,
      currentExplorer.position,
      currentExplorer.value,
      this,
      neighborhoodName,
      newObj)


  def performRemove(): Unit = {
    schP.activitiesPriorList.remove(currentExplorer.position)
  }
}

case class RemoveActivityMove(schP: SchedulingProblem,
                              activityPosition:Int,
                              removedActivity:Int,
                              override val neighborhood: RemoveActivity,
                              override val neighborhoodName: String,
                              override val objAfter: Int)
  extends SchedulingMove(schP, neighborhood, neighborhoodName, objAfter) {

  override def impactedActivities: Iterable[Int] = Some(removedActivity)

  /** to actually take the move */
  override def commit(): Unit = {
    schP.activitiesPriorList.remove(activityPosition)
  }

  override def toString: String =
    neighborhoodName + "::" + "RemoveActivityMove(activity:" + removedActivity + "position:" + activityPosition + objToString + ")"
}

