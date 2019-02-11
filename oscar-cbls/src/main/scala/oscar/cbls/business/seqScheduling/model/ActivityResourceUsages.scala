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
  // Which resources are used by activities
  val activityResources: Array[Set[Int]] = Array.tabulate(numActivities)(_ => Set())

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
    activityResources(act.index) += res.index
  }

  /**
    * Gets the usage data of a resource by an activity (by indices)
    *
    * @param indAct the index of the activity
    * @param indRes the index of the resource
    * @return the data (running mode and used capacity) for the usage of
    *         the resource indRes by the activity indAct, as an Option value
    */
  def getActivityResourceUsage(indAct: Int, indRes: Int): Option[ResourceUsage] = {
    resourceUsages(indAct)(indRes)
  }

  /**
    * Gets the usage data of a resource by an activity
    *
    * @param act the activity
    * @param res the resource
    * @return the data (running mode and used capacity) of the usage of
    *         the resource res by the activity act, as an Option value
    */
  def getActivityResourceUsage(act: Activity, res: Resource): Option[ResourceUsage] = {
    resourceUsages(act.index)(res.index)
  }

  /**
    * Gets the set of usage data for all the resources of one activity
    *
    * @param indAct the index of the activity
    * @return the set of resource usages for the activity indAct
    */
  def getActivityResourceUsages(indAct: Int): Set[ResourceUsage] = {
    for {indRes <- activityResources(indAct)} yield getActivityResourceUsage(indAct, indRes).get
  }

  /**
    * Gets the set of usage data for all the resources of one activity
    *
    * @param act the activity
    * @return he set of resource usages for the activity act
    */
  def getActivityResourceUsages(act: Activity): Set[ResourceUsage] = {
    getActivityResourceUsages(act.index)
  }

}

/**
  * Data of a resource usage
  *
  * @param indexRM the index of the running mode
  * @param capacity the used capacity
  */
case class ResourceUsage(indexRM: Int, capacity: Int)