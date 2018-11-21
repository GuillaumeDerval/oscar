package oscar.cbls.business.seqScheduling.modeling

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.business.seqScheduling.invariants._
import oscar.cbls.business.seqScheduling.model.SchedulingProblem

trait SchedulingInvariants {
  def startTimesActivities(priorityActivitiesList: ChangingSeqValue,
                           schedulingProblem: SchedulingProblem): (CBLSIntVar, Array[CBLSIntVar], SetupTimes) = {
    StartTimesActivities(priorityActivitiesList, schedulingProblem)
  }
}
