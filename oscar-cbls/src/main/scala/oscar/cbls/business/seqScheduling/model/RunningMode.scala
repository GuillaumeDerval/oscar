package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class RunningMode(val m: Store, val name: String, val valDefaultSetupTime: Int) {
  var index: Int = Constants.NO_INDEX
  val defaultSetupTimeCBLS = new CBLSIntVar(m, valDefaultSetupTime, 0 to Int.MaxValue, s"${name}__default_setup_time")

  def defaultSetupTime: Int = defaultSetupTimeCBLS.value
}
