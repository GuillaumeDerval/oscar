package oscar.cbls.business.seqScheduling.model

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
