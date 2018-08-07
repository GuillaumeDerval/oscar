package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class RunningMode__A(val m: Store, val name: String, val valDefaultSetupTime: Int) {
  var index: Int = Constants.NO_INDEX
  val defaultSetupTimeCBLS = new CBLSIntVar(m, valDefaultSetupTime, 0 to Int.MaxValue, s"${name}__default_setup_time")

  def defaultSetupTime: Int = defaultSetupTimeCBLS.value
}



//////////////////////////////////////
/////////////////////////// DEPRECATED
//////////////////////////////////////

/**
  * Classes for running modes
  */

abstract class RunningMode {
  val defaultSetupTime: Int = 0
}

/**
  * This object represents a "no running mode", useful for models without running modes
  */
case object NoRunningMode extends RunningMode

/**
  * This class represents a running mode on resources
  *
  * @param name the name of the running mode
  * @param defaultSetupTime the default setup time for this running mode
  */
case class ResourceMode(name: String, override val defaultSetupTime: Int) extends RunningMode
