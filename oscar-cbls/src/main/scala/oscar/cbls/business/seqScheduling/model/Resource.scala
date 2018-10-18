package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class Resource(val m: Store,
               val name: String,
               val valCapacity: Int,
               initialMode: RunningMode,
               schM: SchedulingModel) {
  var index: Int = Constants.NO_INDEX
  val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__capacity")
  val initialModeIndex: Int = initialMode.index
  val runningModes: Array[Option[RunningMode]] = Array.tabulate(schM.maxResources)(i => {
    if (i == initialModeIndex) Some(initialMode) else None
  })
  val setupTimeModes: Array[Array[Option[Int]]] = Array.tabulate(schM.maxModes)(i => {
    Array.tabulate(schM.maxModes)(j => {
      if ((i < schM.nbModes) && (j < schM.nbModes)) Some(schM.runningModes(j).defaultSetupTime)
      else None
    })
  })

  def capacity: Int = capacityCBLS.value

  def addRunningMode(rm: RunningMode): Unit = {
    runningModes(rm.index) = Some(rm)
    // Update setupTimeModes
    for {i <- 0 until schM.nbModes} {
      setupTimeModes(i)(rm.index) = Some(rm.defaultSetupTime)
    }
  }

  def setSetupTime(rm0: RunningMode, rm1: RunningMode, setupTime: Int): Unit = {
    setupTimeModes(rm0.index)(rm1.index) = Some(setupTime)
  }

  def getRunningModes: List[RunningMode] = runningModes.flatten.toList
}
