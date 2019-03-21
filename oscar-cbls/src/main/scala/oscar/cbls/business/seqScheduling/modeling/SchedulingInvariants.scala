package oscar.cbls.business.seqScheduling.modeling

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.business.seqScheduling.invariants._
import oscar.cbls.business.seqScheduling.model.{SchedulingProblem, SchedulingProblem_B, SetupTimes, SetupTimes_B}

trait SchedulingInvariants {
  def startTimesActivities(priorityActivitiesList: ChangingSeqValue,
                           schedulingProblem: SchedulingProblem): (CBLSIntVar, Array[CBLSIntVar], SetupTimes) = {
    StartTimesActivities(priorityActivitiesList, schedulingProblem)
  }


  //TODO Temp
  def startTimesActivities_B(priorityActivitiesList: ChangingSeqValue,
                             schedulingProblem: SchedulingProblem_B): (CBLSIntVar, Array[CBLSIntVar], SetupTimes_B) = {
    StartTimesActivities_B(priorityActivitiesList, schedulingProblem)
  }
}
