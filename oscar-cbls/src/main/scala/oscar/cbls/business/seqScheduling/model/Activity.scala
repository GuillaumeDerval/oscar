package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class Activity(val m: Store,
               val name: String,
               val valDuration: Int,
               schM: SchedulingModel) {
  var index: Int = Constants.NO_INDEX
  val durationCBLS = new CBLSIntVar(m, valDuration, 0 to Int.MaxValue, s"${name}__duration")
  val resourceUsages: Array[Option[ResourceUsage]] = Array.tabulate(schM.maxResources)(_ => None)

  def duration: Int = durationCBLS.value

  def addResourceUsage(res: Resource, capacity: Int, rm: RunningMode): Unit = {
    resourceUsages(res.index) = Some(new ResourceUsage(res.index, capacity, rm))
  }

  // Internal class representing the usage of a resource
  class ResourceUsage(val resourceIndex: Int, valCapacity: Int, val runningMode: RunningMode) {
    val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__uses_res${resourceIndex}__capacity")

    def capacity: Int = capacityCBLS.value
  }
}