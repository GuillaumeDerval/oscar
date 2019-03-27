package oscar.cbls.business.scheduling.model

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.{CBLSSeqVar, Store}

class Schedule(model: Store,
               actDurations: Array[Long],
               actPrecPairs: List[(Int, Int)],
               initialActivities: Iterable[Int],
               resConstraints: Array[ResourceConstraint]) {
  val numActivities: Int = actDurations.length
  // Precedences
  val precedences = new Precedences(numActivities, actPrecPairs)

  // CBLS variable: the priority list of activities to be scheduled
  val activitiesPriorList = new CBLSSeqVar(model,
                                           IntSequence(precedences.getPriorityList(initialActivities)),
                                           numActivities - 1,
                                           "Activities' Priority List")
  // CBLS invariant: start times
  val (makeSpan, startTimes) = StartTimes(activitiesPriorList, actDurations, precedences, resConstraints)

  /**
    * Given an activity index (indAct), obtain the sequence of indices that
    * can be swapped with indAct in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def swappableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activitiesPriorList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get.toInt
    val predActIndices = precedences.precArray(currentActivity)
    val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
    val succActIndices = precedences.succArray(currentActivity)
    val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = -1
    var firstSuccSeqIndex = prioritySequence.size
    var seqIndexIndAct = -1
    var inLoop = true
    var optExplorer = prioritySequence.explorerAtPosition(0)
    while (inLoop && optExplorer.isDefined) {
      val actExplorer = optExplorer.get
      val i = actExplorer.position
      val activityAtI = actExplorer.value
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inLoop = hasSuccessors
      } else if (seqIndexIndAct == -1 && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != -1) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop loop because we found the first successor
          inLoop = false
        }
      }
      optExplorer = actExplorer.next
    }
    // The swappable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var swappableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      optExplorer = prioritySequence.explorerAtPosition(i)
      val activityAtI = optExplorer.get.value.toInt
      if (i > seqIndexIndAct) {
        // if index i is after indAct, check that there is no predecessor of i
        // between indAct and i, in that case i, indAct are not swappable
        val predecessorsOfI = precedences.precArray(activityAtI).filter(p => prioritySequence.contains(p))
        var noPrecBetweenIndAct_I = true
        for { j <- indAct+1 until i if noPrecBetweenIndAct_I} {
          optExplorer = prioritySequence.explorerAtPosition(j)
          val activityAtJ = optExplorer.get.value
          noPrecBetweenIndAct_I = !predecessorsOfI.contains(activityAtJ)
        }
        if (noPrecBetweenIndAct_I) {
          swappableIndices :+= i
        }
      }
      else {
        // if index i is before indAct, check that there is no successor of i
        // between i and indAct, in that case i, indAct are not swappable
        val successorsOfI = precedences.succArray(activityAtI).filter(s => prioritySequence.contains(s))
        var noSuccBetweenI_IndAct = true
        for { j <- i+1 until seqIndexIndAct if noSuccBetweenI_IndAct} {
          optExplorer = prioritySequence.explorerAtPosition(j)
          val activityAtJ = optExplorer.get.value.toInt
          noSuccBetweenI_IndAct = !successorsOfI.contains(activityAtJ)
        }
        if (noSuccBetweenI_IndAct) {
          swappableIndices :+= i
        }
      }
    }
    swappableIndices
  }

  /**
    * Given an activity index (indAct), obtain the sequence of indices where
    * indAct can be reinserted in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def reinsertableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activitiesPriorList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get.toInt
    val predActIndices = precedences.precArray(currentActivity)
    val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
    val succActIndices = precedences.succArray(currentActivity)
    val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = -1
    var firstSuccSeqIndex = prioritySequence.size
    var seqIndexIndAct = -1
    var inLoop = true
    var optExplorer = prioritySequence.explorerAtPosition(0)
    while (inLoop && optExplorer.isDefined) {
      val actExplorer = optExplorer.get
      val i = actExplorer.position
      val activityAtI = actExplorer.value
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inLoop = hasSuccessors
      } else if (seqIndexIndAct == -1 && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != -1) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop loop because we found the first successor
          inLoop = false
        }
      }
      optExplorer = actExplorer.next
    }
    // The reinsertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var reinsertableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      reinsertableIndices :+= i
    }
    reinsertableIndices
  }

  def insertableIndices(actInd: Long): Iterable[Int] = {
    val prioritySequence = activitiesPriorList.value
    val predActIndices = precedences.precArray(actInd.toInt)
    val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
    val succActIndices = precedences.succArray(actInd.toInt)
    val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = -1
    var firstSuccSeqIndex = prioritySequence.size
    var seqIndexIndAct = -1
    var inLoop = true
    var optExplorer = prioritySequence.explorerAtPosition(0)
    while (inLoop && optExplorer.isDefined) {
      val actExplorer = optExplorer.get
      val i = actExplorer.position
      val activityAtI = actExplorer.value
      // is i the index for the activity actInd ?
      if (activityAtI == actInd) {
        seqIndexIndAct = i
        inLoop = hasSuccessors
      } else if (seqIndexIndAct == -1 && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != -1) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop loop because we found the first successor
          inLoop = false
        }
      }
      optExplorer = actExplorer.next
    }
    // The insertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var insertableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      insertableIndices :+= i
    }
    insertableIndices
  }
}
