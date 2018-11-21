package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.model.{Constants, SchedulingProblem}
import oscar.cbls.core.computation.CBLSSeqVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class SwapActivity(schP: SchedulingProblem,
                   neighborhoodName: String,
                   selectIndiceBehavior:LoopBehavior = First(),
                   selectSwapBehavior:LoopBehavior = First(),
                   searchIndices: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[SwapActivityMove](neighborhoodName) {

  var currentIndex: Int = Constants.NO_INDEX
  var swappingIndex: Int = Constants.NO_INDEX
  var doneMoves: Set[(IntSequence, Int, Int, IntSequence)] = Set()

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    // iteration zone on activities indices
    //TODO: check the hotRestart in other neighborhood; this really speeds up the search in other contexts; possibly in scheduling as well?
    val iterationZone = searchIndices.getOrElse(() => 0 until schP.activities.size)

    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schP.activitiesPriorList.defineCurrentValueAsCheckpoint(true)
    // iterating over the indices in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndiceBehavior.toIterator(iterationZone())

    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next()
      // explore the swappable zone from the current index
      val swappableZone = schP.swappableIndices(currentIndex)
      val (swappableIterator, notifySwappingFound) = selectSwapBehavior.toIterator(swappableZone)
      while (swappableIterator.hasNext) {
        swappingIndex = swappableIterator.next()
        // Check for eventual symmetry
        val leadsToSymmetry = doneMoves.exists(move => {
          // Condition 1 : a "back move" already explored
          val cond1 = move._1.equals(seqValueCheckPoint) &&
            move._3 == swappingIndex &&
            move._2 == currentIndex
          // Condition 2 : a dual swapping already done
          val cond2 = move._4.equals(seqValueCheckPoint) &&
            move._3 == swappingIndex &&
            move._2 == currentIndex
          cond1 || cond2
        })
        if (!leadsToSymmetry) {
          // Perform move on sequence
          performMove(currentIndex, swappingIndex)
          val newObj = obj.value
          // Adds the move to the symmetry structure
          doneMoves += ((schP.activitiesPriorList.value, swappingIndex, currentIndex, seqValueCheckPoint))
          // Notification of finding indices
          if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
            notifyIndexFound()
            notifySwappingFound()
          }
        }
        // Rollback to checkpoint
        schP.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
      }
    }
    schP.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): SwapActivityMove =
    SwapActivityMove(schP, currentIndex, swappingIndex, this, neighborhoodNameToString, newObj)

  def performMove(currentIndex: Int, swappingIndex: Int): Unit = {
    // Swap 1-segments in sequence
    schP.activitiesPriorList.swapSegments(currentIndex, currentIndex, false,
      swappingIndex, swappingIndex, false)
  }
}

case class SwapActivityMove(schP: SchedulingProblem,
                            firstIndex: Int,
                            secondIndex: Int,
                            override val neighborhood: SwapActivity,
                            override val neighborhoodName: String = "SwapActivityMove",
                            override val objAfter: Int)
  extends SchedulingMove(schP, neighborhood, neighborhoodName, objAfter) {
  // The sequence variable
  val activitiesSeq: CBLSSeqVar = schP.activitiesPriorList

  override def impactedActivities: Iterable[Int] = IndexedSeq(firstIndex, secondIndex)

  /** to actually take the move */
  override def commit(): Unit = {
    // swap the values in indices
    activitiesSeq.swapSegments(firstIndex, firstIndex, false, secondIndex, secondIndex, false)
  }
}