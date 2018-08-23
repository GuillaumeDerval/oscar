package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.business.seqScheduling.model.{Constants, SchedulingSolver}
import oscar.cbls.core.computation.CBLSSeqVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class Reinsertion(scm: SchedulingSolver,
                  neighborhoodName: String,
                  selectIndiceBehavior:LoopBehavior = First(),
                  selectSwapBehavior:LoopBehavior = First(),
                  searchIndices: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[ReinsertionMove](neighborhoodName){

  var currentIndex: Int = Constants.NO_INDEX
  var swappingIndex: Int = Constants.NO_INDEX

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    // iteration zone on activities indices
    val iterationZone = searchIndices.getOrElse(() => 0 until scm.scModel.nbActivities)

    // iterating over the indices in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndiceBehavior.toIterator(iterationZone())
    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next()
      // explore the insertable zone of the current indice
      val insertableZone = scm.insertableIndices(currentIndex)
      val (insertableIterator, notifySwappingFound) = selectSwapBehavior.toIterator(insertableZone)
      while (insertableIterator.hasNext) {
        swappingIndex = insertableIterator.next()
        // Notification of finding indices
        notifyIndexFound()
        notifySwappingFound()
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): ReinsertionMove =
    ReinsertionMove(scm, currentIndex, swappingIndex, this, neighborhoodNameToString, newObj)
}

case class ReinsertionMove(scm: SchedulingSolver,
                           firstIndex: Int,
                           secondIndex: Int,
                           override val neighborhood: Reinsertion,
                           override val neighborhoodName: String = "ReinsertionMove",
                           override val objAfter: Int)
  extends SchedulingMove(scm, neighborhood, neighborhoodName, objAfter) {
  // The sequence variable
  val activitiesSeq: CBLSSeqVar = scm.activitiesSequence

  override def impactedActivities: Iterable[Int] = IndexedSeq(firstIndex, secondIndex)

  /** to actually take the move */
  override def commit(): Unit = {
    // activity index changes from firstIndex to secondIndex
    scm.scModel.activities(firstIndex).index = secondIndex
    // activity index changes from secondIndex to firstIndex
    scm.scModel.activities(secondIndex).index = firstIndex
    // swap the values in indices
    val seqVal = activitiesSeq.value
    val indActFirstOpt = seqVal.valueAtPosition(firstIndex)
    if (indActFirstOpt.isDefined) {
      val indActSecondOpt = seqVal.valueAtPosition(secondIndex)
      if (indActSecondOpt.isDefined) {
        // we can perform the swapping
        activitiesSeq.remove(firstIndex)
        activitiesSeq.insertAtPosition(indActSecondOpt.get, firstIndex)
        activitiesSeq.remove(secondIndex)
        activitiesSeq.insertAtPosition(indActFirstOpt.get, secondIndex)
      }
    }
  }
}