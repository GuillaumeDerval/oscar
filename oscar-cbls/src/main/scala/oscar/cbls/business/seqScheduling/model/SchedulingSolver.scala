package oscar.cbls.business.seqScheduling.model

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.invariants.StartTimesActivities
import oscar.cbls.lib.invariant.seq.Precedence

class SchedulingSolver(val m: Store, val scModel: SchedulingModel__A) {
  // CBLS variable representing the Priority List of activities
  val activitiesSequence = new CBLSSeqVar(m, IntSequence(scModel.getPriorityList), scModel.nbActivities-1, "Scheduling Activities")
  // CBLS invariant, precedences
  val precedences = Precedence(activitiesSequence, scModel.precedences.toPairsList)
  // CBLS invariant, start times
  val (startTimes, setupTimes) = StartTimesActivities(activitiesSequence, scModel)

  def insertableIndices(indAct: Int): Iterable[Int] = {
    val actInd = scModel.activities(indAct)
    val predActIndices = scModel.precedences.precArray(indAct)
    val succActIndices = scModel.precedences.succArray(indAct)
    // lower bound of insertable zone: "last" predecessor of indAct
    val lastPredIndex = predActIndices.fold(Constants.NO_INDEX)((acc, ind) =>
      math.max(acc, scModel.activities(ind).index))
    // upper bound of insertable zone: "first" successor of indAct
    val firstSuccIndex = succActIndices.fold(scModel.nbActivities)((acc, ind) =>
      math.min(acc, scModel.activities(ind).index))
    // the insertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    for { i <- lastPredIndex+1 until firstSuccIndex if i != indAct } yield i
  }
}
