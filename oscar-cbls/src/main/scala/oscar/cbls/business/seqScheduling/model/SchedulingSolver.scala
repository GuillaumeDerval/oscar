package oscar.cbls.business.seqScheduling.model

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.lib.invariant.seq.Precedence

class SchedulingSolver(val m: Store, val scm: SchedulingModel__A) {
  // CBLS var List of activities
  val activities = new CBLSSeqVar(m, IntSequence(scm.getPriorityList), scm.nbActivities-1, "Scheduling Activities")
  // CBLS invariant, precedences
  val precedences = Precedence(activities, scm.precedences.toPairsList)
  // CBLS invariant, start times

}
