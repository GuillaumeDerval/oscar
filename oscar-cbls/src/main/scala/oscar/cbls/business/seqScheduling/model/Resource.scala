package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class Resource__A(val m: Store,
                  val name: String,
                  val valCapacity: Int,
                  initialMode: RunningMode__A,
                  schM: SchedulingModel__A) {
  var index: Int = Constants.NO_INDEX
  val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__capacity")
  val initialModeIndex: Int = initialMode.index
  val runningModes: Array[Option[RunningMode__A]] = Array.tabulate(schM.maxResources)(i => {
    if (i == initialModeIndex) Some(initialMode) else None
  })
  val setupTimeModes: Array[Array[Option[Int]]] = Array.tabulate(schM.maxModes)(i => {
    Array.tabulate(schM.maxModes)(j => {
      if ((i < schM.nbModes) && (j < schM.nbModes)) Some(schM.runningModes(j).defaultSetupTime)
      else None
    })
  })


  def capacity: Int = capacityCBLS.value

  def addRunningMode(rm: RunningMode__A): Unit = {
    runningModes(rm.index) = Some(rm)
    // Update setupTimeModes
    for {i <- 0 until schM.nbModes} {
      setupTimeModes(i)(rm.index) = Some(rm.defaultSetupTime)
    }
  }

  def getRunningModes: List[RunningMode__A] = runningModes.flatten.toList
}



//////////////////////////////////////
/////////////////////////// DEPRECATED
//////////////////////////////////////


/**
  * This class represents a generic resource
  *
  * @param name the name of the resource
  * @param maxCapacity the maximum capacity (or number of units) of the resource
  * @param initialMode the initial running mode of the resource
  * @param runningModes all the running modes available for the resource
  * @param setupTimesModes the setup times between running modes in the resource
  * @param numActivities the number of activities in the model (maybe this should be a global variable)
  */
case class Resource(name: String,
                    maxCapacity: Int,
                    initialMode: RunningMode,
                    runningModes: Set[RunningMode],
                    setupTimesModes: Set[SetupTime],
                    numActivities: Int) {
  // Class Preconditions :
  // - The initial mode is in the set of running modes
  // - The setup times are consistent
  require(runningModes.contains(initialMode))
  require(setupTimesModes.forall(stm => {
    (stm.m0 != stm.m1) &&
      (stm.time >= 0) &&
      runningModes.contains(stm.m0) &&
      runningModes.contains(stm.m1)
  }))

  // Usage activities
  val usageActivities: Array[UsageResource] = Array.tabulate(numActivities)(_ => NoUsage)

  // Resources carry a state, which is their consumption across the schedule
  // * Last activity that used this resource
  var lastActivityIndex: Int = Activity.SOURCE
  // * Forward flows after last activity used this resource
  var forwardFlows: List[ResourceFlow] = List(SourceFlow(maxCapacity))
  // * Running mode of last activity that used this resource
  var lastRunningMode: RunningMode = initialMode
  // * Check variable to verify if running mode has changed
  var changedRunningMode: Option[RunningMode] = None

  /**
    * reset the state of the resource
    */
  def resetState(): Unit = {
    lastActivityIndex = Activity.SOURCE
    forwardFlows = List(SourceFlow(maxCapacity))
    lastRunningMode = initialMode
  }

  /**
    * get the setup time between two running modes in this resource
    *
    * @param m0 initial running mode
    * @param m1 final running mode
    * @return the setup time to change mode from m0 to m1 in the resource.
    */
  def setupTimeChangeMode(m0: RunningMode, m1: RunningMode): Int = {
    if (m0 == m1) 0
    else {
      val stMode = setupTimesModes.find(stm => stm.m0==m0 && stm.m1==m1)
      if (stMode.isDefined) stMode.get.time else m1.defaultSetupTime
    }
  }

  override def toString: String = {
    val strUsages = usageActivities.foldLeft("")((str, ur) => s"$str\n   $ur")
    s"  Name: $name\n  Capacity: $maxCapacity\n  Usage Activities:$strUsages\n  Initial Mode: $initialMode\n  Modes: $runningModes\n  Setup Times: $setupTimesModes\n"
  }
}
