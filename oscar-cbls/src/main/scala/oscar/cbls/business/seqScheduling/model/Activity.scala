package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

/**
  * This class represents an Activity (Task, Step, ...) in the scheduling
  * model
  *
  * @param m the CBLS store
  * @param name a readable name for the activity
  * @param valDuration the initial duration of the activity
  * @param schM the scheduling model this activity belongs to
  */
class Activity(val m: Store,
               val name: String,
               val valDuration: Int,
               schM: SchedulingModel) {
  // index in activities array in the schM model
  var index: Int = Constants.NO_INDEX
  // CBLS variable for activity duration
  val durationCBLS = new CBLSIntVar(m, valDuration, 0 to Int.MaxValue, s"${name}__duration")
  // resource usages for this activity
  val resourceUsages: Array[Option[ResourceUsage]] = Array.tabulate(schM.maxResources)(_ => None)

  /**
    * Activity duration
    * @return the current duration of the activity
    */
  def duration: Int = durationCBLS.value

  /**
    * Adds the usage of a resource
    *
    * @param res the used resource
    * @param capacity the capacity of the resource used by the activity
    * @param rm the running mode of the resource when used by the activity
    */
  def addResourceUsage(res: Resource, capacity: Int, rm: RunningMode): Unit = {
    //TODO: je pense que les paramètre de la resource sont spécifiques à la classe de resource considérée. Donc cette mathode derait être dans la classe ressource, et chaque classe de resource pourrait donc avoir une méthode diférente
    resourceUsages(res.index) = Some(new ResourceUsage(res.index, capacity, rm))
  }

  /**
    * Internal class representing the usage of a resource
    *
    * @param resourceIndex the index of the resource
    * @param valCapacity the capacity of the resource used by the activity
    * @param runningMode the running mode of the resource when used by the activity
    */
  class ResourceUsage(val resourceIndex: Int, valCapacity: Int, val runningMode: RunningMode) {
    // CBLS variable representing the capacity of the resource used by this activity
    val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__uses_res${resourceIndex}__capacity")

    /**
      * Capacity of the resource used by the activity
      *
      * @return the current value of the capacity
      */
    def capacity: Int = capacityCBLS.value
  }
}