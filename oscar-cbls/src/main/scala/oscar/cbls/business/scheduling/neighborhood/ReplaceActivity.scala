package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.LoopBehavior
import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First}

class ReplaceActivity(schedule: Schedule,
                      neighborhoodName: String,
                      selectRemoveBehavior:LoopBehavior = First(),
                      selectActToAddBehavior:LoopBehavior = First(),
                      selectReinsertBehavior:LoopBehavior = Best(),
                      searchIndices: Option[() => Iterable[Long]] = None)
  extends EasyNeighborhoodMultiLevel[ReplaceActivityMove](neighborhoodName) {

  var indActToRemove: Int  = -1
  var actToAdd: Long = -1L
  var indActToAdd: Int = -1

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    // Iteration zone on activities indices to remove
    // Checking the Hot Restart
    val iterationZone1: () => Iterable[Long] = searchIndices.getOrElse(() => 0L until schedule.activitiesPriorList.value.size.toLong)
    val hotRestart = true
    val iterationZone: Iterable[Long] =
      if (hotRestart) HotRestart(iterationZone1(), indActToRemove.toLong)
      else iterationZone1()
    // iterating over the values in the activity list
    val (indexIterator, notifyIndexToRemoveFound) = selectRemoveBehavior.toIterator(iterationZone)
    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schedule.activitiesPriorList.defineCurrentValueAsCheckpoint(true)
    while (indexIterator.hasNext) {
      indActToRemove = indexIterator.next().toInt
      // iterating over the possible activities to add
      val iterationZone2: () => Iterable[Long] = () => (0L until schedule.numActivities).filterNot(schedule.activitiesPriorList.value.contains(_))
      val iterationZoneAdding: Iterable[Long] =
        if (hotRestart) HotRestart(iterationZone2(), actToAdd)
        else iterationZone2()
      // iterating over the activities to add
      val (actToAddIterator, notifyActToAddFound) = selectActToAddBehavior.toIterator(iterationZoneAdding)
      while (actToAddIterator.hasNext) {
        actToAdd = actToAddIterator.next()
        // Iteration over the reinsertable zone
        val reinsertableZone = schedule.insertableIndices(actToAdd).map { ind =>
          if (ind > indActToRemove) ind-1
          else ind
        }
        val (reinsertableIterator, notifyRensertFound) = selectReinsertBehavior.toIterator(reinsertableZone)
        while (reinsertableIterator.hasNext) {
          indActToAdd = reinsertableIterator.next()
          // Perform move on sequence
          performMove(indActToRemove, actToAdd, indActToAdd)
          val newObj = obj.value
          // Notification of finding indices
          if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
            notifyIndexToRemoveFound()
            notifyActToAddFound()
            notifyRensertFound()
          }
          // Rollback to checkpoint
          schedule.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
        }
      }
    }
    schedule.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): ReplaceActivityMove =
    ReplaceActivityMove(indActToRemove, actToAdd, indActToAdd, this, neighborhoodNameToString, newObj)

  def performMove(indActToRemove: Int, actToAdd: Long, indActToAdd: Int): Unit = {
    schedule.activitiesPriorList.remove(indActToRemove)
    schedule.activitiesPriorList.insertAtPosition(actToAdd, indActToAdd)
  }
}

case class ReplaceActivityMove(removeIndex: Int,
                               actToAdd: Long,
                               addIndex: Int,
                               override val neighborhood: ReplaceActivity,
                               override val neighborhoodName: String = "ReplaceActivityMove",
                               override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter) {
  override def impactedActivities: Iterable[Int] = {
    val minIndex = math.min(removeIndex, addIndex)
    val maxIndex = math.max(removeIndex, addIndex)
    for { i <- minIndex to maxIndex } yield i
  }

  /** to actually take the move */
  override def commit(): Unit = neighborhood.performMove(removeIndex, actToAdd, addIndex)
}