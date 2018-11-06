package oscar.cbls.business.seqScheduling.model

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.invariants.StartTimesActivities
import oscar.cbls.lib.invariant.seq.Precedence

/**
  * This class encapsulates the operative aspects of a Scheduling problem
  * (Sequence variable representing an ordering between activities,
  * invariants for the precedences and the starting times of activities, ...)
  *
  * @param m the CBLS store
  * @param scModel the scheduling model
  */
class SchedulingSolver(val m: Store, val scModel: SchedulingModel) {
  // CBLS variable representing the Priority List of activities
  val activitiesSequence = new CBLSSeqVar(m, IntSequence(scModel.getPriorityList), scModel.nbActivities-1, "Scheduling Activities")
  // CBLS invariant, precedences
  val precedences = Precedence(activitiesSequence, scModel.precedences.toPairsList)
  // CBLS invariant, start times
  val (makeSpan, startTimes, setupTimes) = StartTimesActivities(activitiesSequence, scModel)
  // Objective function: Makespan
  val mkspObj = Objective(makeSpan)

  /**
    * Given an activity index (indAct), obtain the sequence of indices that
    * can be swapped with indAct in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def insertableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activitiesSequence.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get
    val predActIndices = scModel.precedences.precArray(currentActivity)
    val hasPredecessors = predActIndices.nonEmpty
    val succActIndices = scModel.precedences.succArray(currentActivity)
    val hasSuccessors = succActIndices.nonEmpty
    // Determine the bounds of the insertable zone in the priority sequence
    var lastPrecSeqIndex = Constants.NO_INDEX
    var firstSuccSeqIndex = scModel.nbActivities
    var seqIndexIndAct = Constants.NO_INDEX
    var i = 0
    var inCycle = true
    while (inCycle && i < scModel.nbActivities) {
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
    // The insertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var insertables: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      val activityAtI = prioritySequence.valueAtPosition(i).get
      if (i > seqIndexIndAct) {
        val predecessorsOfI = scModel.precedences.precArray(activityAtI)
        var noPrecBetweenIndAct_I = true
        for { j <- indAct+1 until i if noPrecBetweenIndAct_I} {
          val activityAtJ = prioritySequence.valueAtPosition(j).get
          noPrecBetweenIndAct_I = !predecessorsOfI.contains(activityAtJ)
        }
        if (noPrecBetweenIndAct_I) {
          insertables :+= i
        }
      }
      else {

        val successorsOfI = scModel.precedences.succArray(activityAtI)
        var noSuccBetweenI_IndAct = true
        for { j <- i+1 until seqIndexIndAct if noSuccBetweenI_IndAct} {
          val activityAtJ = prioritySequence.valueAtPosition(j).get
          noSuccBetweenI_IndAct = !successorsOfI.contains(activityAtJ)
        }
        if (noSuccBetweenI_IndAct) {
          insertables :+= i
        }
      }
    }
    insertables
  }
}
