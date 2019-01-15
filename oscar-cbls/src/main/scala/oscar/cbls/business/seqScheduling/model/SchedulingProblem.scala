package oscar.cbls.business.seqScheduling.model

import oscar.cbls.{CBLSSeqVar, Objective, Store}
import oscar.cbls.algo.boundedArray.BoundedArray
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.invariants.StartTimesActivities

/**
  * Model of a scheduling problem
  *
  * @param m the CBLS store
  * @param activities the activities
  * @param resources the resources
  * @param precedences the precedence relation
  * @param activityResourceUsages the usage of resources by activities
  */
class SchedulingProblem(val m: Store,
                        val activities: BoundedArray[Activity],
                        val resources: BoundedArray[Resource],
                        val precedences: Precedences,
                        val activityResourceUsages: ActivityResourceUsages) {
  // CBLS variable representing the Priority List of activities
  val activitiesPriorList = new CBLSSeqVar(m, IntSequence(getPriorityList), activities.size-1, "Scheduling Activities Priority List")
  // CBLS invariant, start times
  val (makeSpan, startTimes, setupTimes) = StartTimesActivities(activitiesPriorList, this)
  // Objective function: Makespan
  val mkspObj = Objective(makeSpan)

  /**
    * Gets a valid priority list from the precedences relation
    *
    * @return a list representing a permutation of [0..nbActivities) where
    *         each element (actI) is an activity index and all indices of successive
    *         activities of the element are found after actI
    */
  def getPriorityList: List[Int] = {
    val prL = precedences.getPriorityList
    println(s"Initial Priority List = $prL")
    prL
  } //precedences.getPriorityList

  /**
    * Given an activity index (indAct), obtain the sequence of indices that
    * can be swapped with indAct in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def swappableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activitiesPriorList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get
    val predActIndices = precedences.precArray(currentActivity)
    val hasPredecessors = predActIndices.nonEmpty
    val succActIndices = precedences.succArray(currentActivity)
    val hasSuccessors = succActIndices.nonEmpty
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = Constants.NO_INDEX
    var firstSuccSeqIndex = activities.size
    var seqIndexIndAct = Constants.NO_INDEX
    var i = 0
    var inCycle = true
    while (inCycle && i < activities.size) {
      val activityAtI = prioritySequence.valueAtPosition(i).get
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inCycle = hasSuccessors
      } else if (seqIndexIndAct == Constants.NO_INDEX && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != Constants.NO_INDEX) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop cycle because we found the first successor
          inCycle = false
        }
      }
      i += 1
    }
    // The swappable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var swappableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      val activityAtI = prioritySequence.valueAtPosition(i).get
      if (i > seqIndexIndAct) {
        // if index i is after indAct, check that there is no predecessor
        // between indAct and i, in that case i, indAct are not swappable
        val predecessorsOfI = precedences.precArray(activityAtI)
        var noPrecBetweenIndAct_I = true
        for { j <- indAct+1 until i if noPrecBetweenIndAct_I} {
          val activityAtJ = prioritySequence.valueAtPosition(j).get
          noPrecBetweenIndAct_I = !predecessorsOfI.contains(activityAtJ)
        }
        if (noPrecBetweenIndAct_I) {
          swappableIndices :+= i
        }
      }
      else {
        // if index i is before indAct, check that there is no successor
        // between i and indAct, in that case i, indAct are not swappable
        val successorsOfI = precedences.succArray(activityAtI)
        var noSuccBetweenI_IndAct = true
        for { j <- i+1 until seqIndexIndAct if noSuccBetweenI_IndAct} {
          val activityAtJ = prioritySequence.valueAtPosition(j).get
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
    val currentActivity = prioritySequence.valueAtPosition(indAct).get
    val predActIndices = precedences.precArray(currentActivity)
    val hasPredecessors = predActIndices.nonEmpty
    val succActIndices = precedences.succArray(currentActivity)
    val hasSuccessors = succActIndices.nonEmpty
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = Constants.NO_INDEX
    var firstSuccSeqIndex = activities.size
    var seqIndexIndAct = Constants.NO_INDEX
    var i = 0
    var inCycle = true
    while (inCycle && i < activities.size) {
      val activityAtI = prioritySequence.valueAtPosition(i).get
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inCycle = hasSuccessors
      } else if (seqIndexIndAct == Constants.NO_INDEX && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != Constants.NO_INDEX) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop cycle because we found the first successor
          inCycle = false
        }
      }
      i += 1
    }
    // The reinsertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var reinsertableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      reinsertableIndices :+= i
    }
    reinsertableIndices
  }
}
