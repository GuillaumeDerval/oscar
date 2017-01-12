package oscar.cbls.lib.invariant.routing.draft

import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

/**
 * Created by rdl on 11-08-16.
 */
class TimeWindow(routes:ChangingSeqValue,
                 v:Int,
                 startTimeWindow:Array[Int],
                 deadline:Array[Int],
                 durationAtNode:Array[Int],
                 maxWaitingTime:Int,
                 violation:Array[CBLSIntVar]) {

  //algo FTS


}
