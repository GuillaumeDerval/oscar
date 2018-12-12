package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.model.{Constants, SchedulingProblem}
import oscar.cbls.core.computation.CBLSSeqVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class ReinsertActivity(schP: SchedulingProblem,
                       neighborhoodName: String,
                       selectIndiceBehavior:LoopBehavior = First(),
                       selectReinsertBehavior:LoopBehavior = First(),
                       searchIndices: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[ReinsertActivityMove](neighborhoodName) {

  var currentIndex: Int = Constants.NO_INDEX
  var reinsertIndex: Int = Constants.NO_INDEX
  var doneMoves: Set[(IntSequence, Int, Int, IntSequence)] = Set()

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    // iteration zone on activities indices
    // Checking the Hot Restart
    val iterationZone1 = searchIndices.getOrElse(() => 0 until schP.activities.size)
    val hotRestart = true
    val iterationZone =
      if (hotRestart) HotRestart(iterationZone1(), currentIndex)
      else iterationZone1()

    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schP.activitiesPriorList.defineCurrentValueAsCheckpoint(true)
    // iterating over the indices in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndiceBehavior.toIterator(iterationZone)

    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next()
      // explore the insertable zone of the current index
      val reinsertableZone = schP.reinsertableIndices(currentIndex)
      val (reinsertableIterator, notifyReinsertFound) = selectReinsertBehavior.toIterator(reinsertableZone)
      while (reinsertableIterator.hasNext) {
        reinsertIndex = reinsertableIterator.next()
        // Check for eventual symmetry
        val leadsToSymmetry = doneMoves.exists(move => {
          // Condition 1 : a "back move" already explored
          val cond1 = move._1.equals(seqValueCheckPoint) &&
            move._3 == reinsertIndex &&
            move._2 == currentIndex
          // Condition 2 : reinserting between consecutive indices
          // where the "dual" move has been already explored
          val cond2 = move._4.equals(seqValueCheckPoint) &&
            move._3 == reinsertIndex &&
            move._2 == currentIndex &&
            math.abs(currentIndex - reinsertIndex) == 1
          cond1 || cond2
        })
        if (!leadsToSymmetry) {
          // Perform move on sequence
          performMove(currentIndex, reinsertIndex)
          val newObj = obj.value
          // Adds the move to the symmetry structure
          doneMoves += ((schP.activitiesPriorList.value, reinsertIndex, currentIndex, seqValueCheckPoint))
          // Notification of finding indices
          if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
            notifyIndexFound()
            notifyReinsertFound()
          }
        }
        // Rollback to checkpoint
        schP.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
      }
    }
    schP.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): ReinsertActivityMove =
    ReinsertActivityMove(schP, currentIndex, reinsertIndex, this, neighborhoodNameToString, newObj)

  def performMove(currentIndex: Int, reinsertIndex: Int): Unit = {
    if (currentIndex < reinsertIndex) {
      schP.activitiesPriorList.swapSegments(currentIndex, currentIndex, false,
        currentIndex+1, reinsertIndex, false)
    }
    else {
      schP.activitiesPriorList.swapSegments(reinsertIndex, currentIndex-1, false,
        currentIndex, currentIndex, false)
    }
  }
}

case class ReinsertActivityMove(schP: SchedulingProblem,
                                currentIndex: Int,
                                reinsertIndex: Int,
                                override val neighborhood: ReinsertActivity,
                                override val neighborhoodName: String = "ReinsertActivityMove",
                                override val objAfter: Int)
  extends SchedulingMove(schP, neighborhood, neighborhoodName, objAfter) {
  // The sequence variable
  val activitiesSeq: CBLSSeqVar = schP.activitiesPriorList

  override def impactedActivities: Iterable[Int] = {
    val minIndex = math.min(currentIndex, reinsertIndex)
    val maxIndex = math.max(currentIndex, reinsertIndex)
    for { i <- minIndex to maxIndex } yield i
  }

  /** to actually take the move */
  override def commit(): Unit = {
    if (currentIndex < reinsertIndex) {
      activitiesSeq.swapSegments(currentIndex, currentIndex, false, currentIndex+1, reinsertIndex, false)
    }
    else {
      activitiesSeq.swapSegments(reinsertIndex, currentIndex-1, false, currentIndex, currentIndex, false)
    }
  }
}