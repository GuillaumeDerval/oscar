package oscar.cbls.business.seqScheduling.model

import oscar.cbls.Store
import oscar.cbls.core.computation.CBLSIntVar

class Activity__A(val m: Store,
                  val name: String,
                  val valDuration: Int,
                  schM: SchedulingModel__A) {
  var index: Int = Constants.NO_INDEX
  val durationCBLS = new CBLSIntVar(m, valDuration, 0 to Int.MaxValue, s"${name}__duration")
  val resourceUsages: Array[Option[ResourceUsage]] = Array.tabulate(schM.maxResources)(_ => None)

  def duration: Int = durationCBLS.value

  def addResourceUsage(res: Resource__A, capacity: Int, rm: RunningMode__A): Unit = {
    resourceUsages(res.index) = Some(new ResourceUsage(res.index, capacity, rm))
  }

  // Internal class representing the usage of a resource
  class ResourceUsage(val resourceIndex: Int, valCapacity: Int, val runningMode: RunningMode__A) {
    val capacityCBLS = new CBLSIntVar(m, valCapacity, 0 to Int.MaxValue, s"${name}__uses_res${resourceIndex}__capacity")

    def capacity: Int = capacityCBLS.value
  }
}


//////////////////////////////////////
/////////////////////////// DEPRECATED
//////////////////////////////////////

/**
  * This class represents an generic activity
  *
  * @param name the name of the activity
  * @param duration the number of time units needed to perform the activity
  */
case class Activity(name: String, duration: Int) {
  var index: Int = Activity.NO_INDEX
  var resources: Vector[Resource] = Vector()
}

/**
  * This object contains basic constants for activities
  */
object Activity {
  final val NO_INDEX = -1
  final val SOURCE = -1
}