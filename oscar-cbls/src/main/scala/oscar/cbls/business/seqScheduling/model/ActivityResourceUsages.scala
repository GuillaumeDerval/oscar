package oscar.cbls.business.seqScheduling.model

/**
  * This class is a container for the usage of the resources by the activities
  * in a scheduling problem
  *
  * @param numActivities the number of activities
  * @param numResources the number of
  */
class ActivityResourceUsages(numActivities: Int, numResources: Int) {
  // Resource usage matrix
  val resourceUsages: Array[Array[Option[ResourceUsage]]] = Array.tabulate(numActivities)(_ =>
    Array.tabulate(numResources)(_ => None)
  )

  /**
    * Adds the usage of a resource by an activity
    *
    * @param act the activity
    * @param res the resource
    * @param rm the running mode
    * @param usedCapacity the used capacity
    */
  def addActivityResourceUsage(act: Activity,
                               res: Resource,
                               rm: RunningMode,
                               usedCapacity: Int): Unit = {
    require(0 <= act.index)
    require(act.index < numActivities)
    require(0 <= res.index)
    require(res.index < numResources)
    require(0 <= rm.index)
    require(rm.index < res.runningModes.size)
    require(0 <= usedCapacity)
    require(usedCapacity <= res.capacity)
    /////
    resourceUsages(act.index)(res.index) = Some(ResourceUsage(rm.index, usedCapacity))
  }

  /**
    * Gets the usage data of a resource by an activity (by indices)
    *
    * @param indAct the index of the activity
    * @param indRes the index of the resource
    * @return the data (running mode and used capacity) of the usage of
    *         the resource indRes by the activity indAct
    */
  def getActivityResourceUsage(indAct: Int, indRes: Int): ResourceUsage = {
    resourceUsages(indAct)(indRes).get
  }

  /**
    * Gets the usage data of a resource by an activity
    *
    * @param act the activity
    * @param res the resource
    * @return the data (running mode and used capacity) of the usage of
    *         the resource res by the activity act
    */
  def getActivityResourceUsage(act: Activity, res: Resource): ResourceUsage = {
    resourceUsages(act.index)(res.index).get
  }

}

/**
  * Data of a resource usage
  *
  * @param indexRM the index of the running mode
  * @param capacity the used capacity
  */
case class ResourceUsage(indexRM: Int, capacity: Int)