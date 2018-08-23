package oscar.cbls.business.seqScheduling.modeling

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.business.seqScheduling.invariants._
import oscar.cbls.business.seqScheduling.model.SchedulingModel__A

trait SchedulingInvariants {
  def startTimesActivities(priorityActivitiesList: ChangingSeqValue,
                           schedulingModel: SchedulingModel__A): (Array[CBLSIntVar], SetupTimes) = {
    StartTimesActivities(priorityActivitiesList, schedulingModel)
  }
}
