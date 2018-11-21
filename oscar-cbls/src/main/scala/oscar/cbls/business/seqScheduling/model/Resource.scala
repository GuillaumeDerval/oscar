package oscar.cbls.business.seqScheduling.model

import oscar.cbls._

/**
  * This class represents a "consommable" resource for activities in the
  * scheduling model
  *
  * @param name a readable name for this resource
  * @param valCapacity the maximal capacity of the resource
  * @param schP the scheduling model this resource belongs to
  */
class Resource(m: Store,
               val name: String,
               val valCapacity: Int,
               val runningModes: RunningModeResources) {
  // index in resources array in the schM model
  var index: Int = Constants.NO_INDEX
  // CBLS var for the maximum capacity
  //TODO: si il y a une variable, c'est un input ou un output? il faut la lier avec les algo de scheduling alors?
  val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__capacity")

  /**
    * Maximal capacity of this resource
    *
    * @return the current value of maximal capacity
    */
  def capacity: Int = capacityCBLS.value

  /**
    * Gets a list of the used running modes
    * @return the running modes used by this resource
    */
  def getRunningModes: List[RunningMode] = runningModes.getRunningModes
}

object Resource {
  def setIndex(res: Resource, index: Int): Unit = {
    res.index = index
  }
}
