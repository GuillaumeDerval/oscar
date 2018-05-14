package oscar.cbls.business.seqScheduling.model

/**
  * Abstract class representing the usage of a resource by an activity
  */
abstract class UsageResource {
  val usesResource: Boolean
  val capacity: Int
  val runningMode: RunningMode
}

/**
  * This object specifies a "No usage" of a resource
  */
case object NoUsage extends UsageResource {
  override val usesResource: Boolean = false
  override val capacity: Int = 0
  override val runningMode: RunningMode = NoRunningMode
}

/**
  * This class represents a specific usage of a resource
  *
  * @param capacity the capacity of the resource used by the activity
  * @param runningMode the running mode of the resource in the activity
  */
case class ActivityResource(capacity: Int, runningMode: RunningMode) extends UsageResource {
  override val usesResource: Boolean = true
}
