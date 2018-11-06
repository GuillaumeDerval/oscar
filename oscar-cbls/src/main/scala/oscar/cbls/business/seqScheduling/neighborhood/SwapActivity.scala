package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.business.seqScheduling.model.{Constants, SchedulingSolver}
import oscar.cbls.core.computation.CBLSSeqVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class SwapActivity(scm: SchedulingSolver,
                   neighborhoodName: String,
                   selectIndiceBehavior:LoopBehavior = First(),
                   selectSwapBehavior:LoopBehavior = First(),
                   searchIndices: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[SwapActivityMove](neighborhoodName){

  var currentIndex: Int = Constants.NO_INDEX
  var swappingIndex: Int = Constants.NO_INDEX

  println(s"Initial makespan = ${scm.makeSpan}")

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {

    println(s"*** Call to explore neighborhood")

    // iteration zone on activities indices
    val iterationZone = searchIndices.getOrElse(() => 0 until scm.scModel.nbActivities)

    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = scm.activitiesSequence.defineCurrentValueAsCheckpoint(true)

    println(s"Objective variable = $obj")
    println(s"Current makespan = ${scm.makeSpan}")

    // iterating over the indices in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndiceBehavior.toIterator(iterationZone())
    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next()
      // explore the insertable zone of the current indice
      val insertableZone = scm.insertableIndices(currentIndex)

      print(s"Insertable zone for $currentIndex = [ ")
      insertableZone.foreach(a => print(s"$a "))
      println("]")

      val (insertableIterator, notifySwappingFound) = selectSwapBehavior.toIterator(insertableZone)
      while (insertableIterator.hasNext) {
        swappingIndex = insertableIterator.next()
        // Perform move on sequence

        println(s"Current Index = $currentIndex | Swapping Index = $swappingIndex")
        println(s"Old obj = ${obj.value}")
        println(s"Sequence before = ${scm.activitiesSequence.value}")
        println(s"Makespan before = ${scm.makeSpan.value}")
        println(s"Start Times before = ${scm.startTimes.foldLeft("[")((acc, stv) => s"$acc ${stv.value}")} ]")
        println(s"Setup Times before = ${scm.setupTimes}")

        performMove(currentIndex, swappingIndex)
        val newObj = obj.value

        println(s"New obj = $newObj")
        println(s"Sequence after = ${scm.activitiesSequence.value}")
        println(s"Makespan after = ${scm.makeSpan.value}")
        println(s"Start Times after = ${scm.startTimes.foldLeft("[")((acc, stv) => s"$acc ${stv.value}")} ]")
        println(s"Setup Times after = ${scm.setupTimes}")

        scm.activitiesSequence.rollbackToTopCheckpoint(seqValueCheckPoint)

        // Notification of finding indices
        if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {

          println(s"Notifying improving move on ${newObj}")

          notifyIndexFound()
          notifySwappingFound()
        }

      }
    }

    scm.activitiesSequence.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Int): SwapActivityMove =
    SwapActivityMove(scm, currentIndex, swappingIndex, this, neighborhoodNameToString, newObj)

  def performMove(currentIndex: Int, swappingIndex: Int): Unit = {
    // Swap 1-segments in sequence
    scm.activitiesSequence.swapSegments(currentIndex, currentIndex, false,
      swappingIndex, swappingIndex, false)
  }
}

case class SwapActivityMove(scm: SchedulingSolver,
                            firstIndex: Int,
                            secondIndex: Int,
                            override val neighborhood: SwapActivity,
                            override val neighborhoodName: String = "SwapActivityMove",
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
    activitiesSeq.swapSegments(firstIndex, firstIndex, false, secondIndex, secondIndex, false)
  }
}