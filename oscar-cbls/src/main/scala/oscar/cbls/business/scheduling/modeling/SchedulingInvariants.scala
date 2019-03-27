package oscar.cbls.business.scheduling.modeling

import oscar.cbls.CBLSIntVar
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.business.scheduling.model.{Precedences, ResourceConstraint}
import oscar.cbls.core.ChangingSeqValue

trait SchedulingInvariants {
  def startTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Array[Long],
                 actPrecedences: Precedences,
                 resourceConstraints: Array[ResourceConstraint]): (CBLSIntVar, Array[CBLSIntVar]) = {
    StartTimes(actPriorityList, actDurations, actPrecedences, resourceConstraints)
  }
}
