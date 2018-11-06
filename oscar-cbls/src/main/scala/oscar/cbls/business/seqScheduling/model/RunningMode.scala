package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

/**
  * This class represents a running mode (configuration) for a given resource
  * in an activity on the scheduling model
  *
  * @param m the CBLS store
  * @param name a readable name for this running mode
  * @param valDefaultSetupTime a default setup time for any other running mode to
  *                            this running mode
  */
class RunningMode(val m: Store, val name: String, val valDefaultSetupTime: Int) {
  // index in the running modes array in the schM model
  var index: Int = Constants.NO_INDEX

  // CBLS variable for the default setup timeµ
  //TODO: a variable, so you must connect it to something in your algo?
  val defaultSetupTimeCBLS = new CBLSIntVar(m, valDefaultSetupTime, 0 to Int.MaxValue, s"${name}__default_setup_time")

  /**
    * gets the default setup time for this running mode
    * @return the current value of default setup time
    */
  def defaultSetupTime: Int = defaultSetupTimeCBLS.value
}
