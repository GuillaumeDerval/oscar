package oscar.cbls.business.seqScheduling.model

import oscar.cbls.algo.boundedArray.BoundedArray

/**
  * This class is a container for the running modes of a resource
  *
  * @param maxModes the maximum number of nodes
  */
class RunningModeResources(maxModes: Int) {
  // The running modes
  private val runningModes: BoundedArray[RunningMode] = new BoundedArray[RunningMode](maxModes,
    RunningMode.setIndex)
  // The setup times
  private val setupTimes: Array[Array[Option[Int]]] = Array.tabulate(maxModes)(i =>
    Array.tabulate(maxModes)(j => if (i == j) Some(0) else None)
  )
  // The index of the initial mode
  var initialModeIndex = Constants.NO_INDEX

  /**
    * Adds a running mode to the container
    *
    * @param rm the running mode
    * @param setInitial a flag for setting rm as the initial
    *                   running mode for the resource
    */
  def addRunningMode(rm: RunningMode, setInitial: Boolean = false): Unit = {
    runningModes :+ rm
    if (setInitial || initialModeIndex == Constants.NO_INDEX) {
      initialModeIndex = rm.index
    }
  }

  /**
    * Adds a setup time between two running modes (by the index)
    *
    * @param indexRM1 the index of first running mode
    * @param indexRM2 the index of second running mode
    * @param setupTime the setup time
    */
  private def addSetupTime(indexRM1: Int, indexRM2: Int, setupTime: Int): Unit = {
    require(0 <= indexRM1)
    require(indexRM1 < runningModes.size)
    require(0 <= indexRM2)
    require(indexRM2 < runningModes.size)
    require(indexRM1 != indexRM2)
    /////
    setupTimes(indexRM1)(indexRM2) = Some(setupTime)
  }

  /**
    * Adds a setup time between two running modes
    *
    * @param rm1 the first (initial) running mode
    * @param rm2 the second (final) running mode
    * @param setupTime the setup time
    */
  def addSetupTime(rm1: RunningMode, rm2: RunningMode, setupTime: Int): Unit = {
    addSetupTime(rm1.index, rm2.index, setupTime)
  }

  /**
    * Sets a running mode as the initial mode (by index)
    *
    * @param indexRM the index of the running mode to be set as initial
    */
  def setInitialMode(indexRM: Int): Unit = {
    require(0 <= indexRM)
    require(indexRM < runningModes.size)
    /////
    initialModeIndex = indexRM
  }

  /**
    * Gets the setup time between two running modes (by index)
    *
    * @param indexRM1 the index of the initial running mode
    * @param indexRM2 the index of the final running mode
    * @return the setup time to change mode from rm1 to rm2
    */
  def setupTime(indexRM1: Int, indexRM2: Int): Int = {
    setupTimes(indexRM1)(indexRM2).getOrElse(runningModes.elementAt(indexRM2).defaultSetupTime)
  }

  /**
    * Gets the running mode at an index
    *
    * @param indexRM the index of the running mode
    * @return the running mode indexed by indexRM
    */
  def getRunningModeAt(indexRM: Int): RunningMode = {
    require(0 <= indexRM)
    require(indexRM < runningModes.size)
    /////
    runningModes.elementAt(indexRM)
  }

  /**
    * Running modes in this container
    *
    * @return the list of running modes in this container
    */
  def getRunningModes: Iterable[RunningMode] = runningModes.toIterable

  /**
    * Size of this container
    *
    * @return the number of running modes in this container
    */
  def size: Int = runningModes.size

  override def toString: String = {
    s"RunningModes:\n  ${runningModes.toIterable.foldLeft("")((acc,rm) => s"$acc  $rm\n")}"
  }
}