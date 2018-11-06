package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

/**
  * This class represents a "consommable" resource for activities in the
  * scheduling model
  *
  * @param m the CBLS store
  * @param name a readable name for this resource
  * @param valCapacity the maximal capacity of the resource
  * @param initialMode the initial running mode for this resource
  * @param schM the scheduling model this resource belongs to
  */
class Resource(val m: Store,
               val name: String,
               val valCapacity: Int,
               initialMode: RunningMode,
               schM: SchedulingModel) {
  // index in resources array in the schM model
  var index: Int = Constants.NO_INDEX
  // CBLS var for the maximum capacity
  val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__capacity")
  // index of initial mode
  val initialModeIndex: Int = initialMode.index
  // running modes for this resource
  val runningModes: Array[Option[RunningMode]] = Array.tabulate(schM.maxResources)(i => {
    if (i == initialModeIndex) Some(initialMode) else None
  })
  // setup times between running modes for this resource
  val setupTimeModes: Array[Array[Option[Int]]] = Array.tabulate(schM.maxModes)(i => {
    Array.tabulate(schM.maxModes)(j => {
      if ((i < schM.nbModes) && (j < schM.nbModes)) Some(schM.runningModes(j).defaultSetupTime)
      else None
    })
  })

  /**
    * Maximal capacity of this resource
    *
    * @return the current value of maximal capacity
    */
  def capacity: Int = capacityCBLS.value

  /**
    * Adds a running mode to the resource
    *
    * @param rm the running mode used by the resource
    */
  def addRunningMode(rm: RunningMode): Unit = {
    runningModes(rm.index) = Some(rm)
    // Update setupTimeModes
    for {i <- 0 until schM.nbModes} {
      setupTimeModes(i)(rm.index) = Some(rm.defaultSetupTime)
    }
  }

  /**
    * Sets the setup time between two running modes
    *
    * @param rm0 the initial running mode
    * @param rm1 the final running mode
    * @param setupTime the value for setup time
    */
  def setSetupTime(rm0: RunningMode, rm1: RunningMode, setupTime: Int): Unit = {
    setupTimeModes(rm0.index)(rm1.index) = Some(setupTime)
  }

  /**
    * Gets a list of the used running modes
    * @return the running modes used by this resource
    */
  def getRunningModes: List[RunningMode] = runningModes.flatten.toList
}
