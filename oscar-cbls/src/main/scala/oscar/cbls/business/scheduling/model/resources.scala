package oscar.cbls.business.scheduling.model

import oscar.cbls.algo.rb.RedBlackTreeMap

// Resource constraints
// Model the resources

abstract class ResourceConstraint {
  def usingActivities: Iterable[Int]
  def initialState: ResourceState
}

class DisjunctiveResource(val usingActivities: Iterable[Int]) extends ResourceConstraint {
  override def initialState: ResourceState = new DisjunctiveResourceState(this, 0L)
}

class DisjunctiveResourceWithSetupTimes(val usingActivities: Iterable[Int],
                                        setupTimes: SetupTimes) extends ResourceConstraint {
  override def initialState: ResourceState =
    new DisjunctiveResourceWithSetupTimesState(this, 0L, setupTimes, setupTimes.initialMode)
}

class CumulativeResource(capacity: Long,
                         val mapConsumingActivities: Map[Int, Long])
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = mapConsumingActivities.keys

  override def initialState: ResourceState = {
    val rbTreeMap: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
    new CumulativeResourceState(this, rbTreeMap.insert(0, capacity))
  }
}

class CumulativeResourceWithSetupTimes(val capacity: Long,
                                       val mapConsumingActivities: Map[Int, Long],
                                       setupTimes: SetupTimes)
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = mapConsumingActivities.keys

  override def initialState: ResourceState = {
    val rbTreeMap: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
    new CumulativeResourceWithSetupTimesState(this, rbTreeMap.insert(0, capacity), setupTimes, setupTimes.initialMode)
  }
}

class ConsumableResource(capacity: Long,
                         val mapConsumingActivities: Map[Int, Long]) extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = mapConsumingActivities.keys

  override def initialState: ResourceState = new ConsumableResourceState(this, 0L, capacity)
}

class ConsumableResourceWithSetupTimes(capacity: Long,
                                       val mapConsumingActivities: Map[Int, Long],
                                       setupTimes: SetupTimes)
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = mapConsumingActivities.keys

  override def initialState: ResourceState = new ConsumableResourceWithSetupTimesState(this, 0L, capacity, setupTimes, setupTimes.initialMode)
}

// Resource States
// These classes model the evolution of a resource through the schedule

abstract class ResourceState {
  def earliestStartTime(activity: Int, earliestStart: Long): Long
  def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState
}

class DisjunctiveResourceState(base: DisjunctiveResource,
                               availableTime: Long)
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    availableTime max earliestStart
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    new DisjunctiveResourceState(base, startTime + taskDuration)
  }
}

class DisjunctiveResourceWithSetupTimesState(base: DisjunctiveResourceWithSetupTimes,
                                             availableTime: Long,
                                             setupTimes: SetupTimes,
                                             currentMode: Int)
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    // Check whether there is a mode change
    val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      availableTime max earliestStart
    } else {
      val setupTime = setupTimes.mapSetupTimes.getOrElse((currentMode, actMode), 0L)
      (availableTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
    new DisjunctiveResourceWithSetupTimesState(base, startTime + taskDuration, setupTimes, actMode)
  }
}

class CumulativeResourceState(base: CumulativeResource,
                              mapTimeReleases: RedBlackTreeMap[Long])
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    // Explore the RB Tree to obtain the release time of enough resources
    var rbExplorerOpt = mapTimeReleases.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceToConsume) {
      releasedTime = rbExplorerOpt.get.key
      releasedQty += rbExplorerOpt.get.value
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    releasedTime max earliestStart
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    val qtyResourceConsumed = base.mapConsumingActivities(activity)
    // Explore the RB Tree to have the keys to delete
    var rbExplorerOpt = mapTimeReleases.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    var lastQty = 0L
    var keysToDelete: List[Long] = Nil
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceConsumed) {
      releasedTime = rbExplorerOpt.get.key
      releasedQty += rbExplorerOpt.get.value
      lastQty = rbExplorerOpt.get.value
      keysToDelete ::= releasedTime
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    // Delete the keys
    var newMapReleases = mapTimeReleases
    keysToDelete.foreach { key =>
      newMapReleases = newMapReleases.remove(key)
    }
    // Add the key for last time if quantity was overloaded
    if (releasedQty > qtyResourceConsumed) {
      newMapReleases = newMapReleases.insert(releasedTime, releasedQty-qtyResourceConsumed)
    }
    // Add the key for the end time of the activity
    newMapReleases = newMapReleases.insert(startTime + taskDuration, qtyResourceConsumed)
    // Return the new state
    new CumulativeResourceState(base, newMapReleases)
  }
}

class CumulativeResourceWithSetupTimesState(base: CumulativeResourceWithSetupTimes,
                                            mapTimeReleases: RedBlackTreeMap[Long],
                                            setupTimes: SetupTimes,
                                            currentMode: Int)
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    // Explore the RB Tree to obtain the release time of enough resources
    var rbExplorerOpt = mapTimeReleases.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceToConsume) {
      releasedTime = rbExplorerOpt.get.key
      releasedQty += rbExplorerOpt.get.value
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    // Check whether there is a mode change
    val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      releasedTime max earliestStart
    } else {
      val setupTime = setupTimes.mapSetupTimes.getOrElse((currentMode, actMode), 0L)
      (releasedTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    val qtyResourceConsumed = base.mapConsumingActivities(activity)
    // Check whether there is a mode change
    val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      // No mode change: explore the RB Tree to have the keys to delete
      var rbExplorerOpt = mapTimeReleases.smallestPosition
      var releasedTime = 0L
      var releasedQty = 0L
      var lastQty = 0L
      var keysToDelete: List[Long] = Nil
      while (rbExplorerOpt.isDefined && releasedQty < qtyResourceConsumed) {
        releasedTime = rbExplorerOpt.get.key
        releasedQty += rbExplorerOpt.get.value
        lastQty = rbExplorerOpt.get.value
        keysToDelete ::= releasedTime
        rbExplorerOpt = rbExplorerOpt.get.next
      }
      // Delete the keys
      var newMapReleases = mapTimeReleases
      keysToDelete.foreach { key =>
        newMapReleases = newMapReleases.remove(key)
      }
      // Add the key for last time if quantity was overloaded
      if (releasedQty > qtyResourceConsumed) {
        newMapReleases = newMapReleases.insert(releasedTime, releasedQty-qtyResourceConsumed)
      }
      // Add the key for the end time of the activity
      newMapReleases = newMapReleases.insert(startTime + taskDuration, qtyResourceConsumed)
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, newMapReleases, setupTimes, actMode)
    } else {
      // Mode has changed:
      val newMapReleases: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, newMapReleases.insert(startTime + taskDuration, base.capacity), setupTimes, actMode)
    }
  }
}

class ConsumableResourceState(base: ConsumableResource,
                              availableTime: Long,
                              availableCapacity: Long) extends ResourceState {

  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    if (qtyResourceToConsume > availableCapacity) {
      // Not available resource, activity will never start
      Long.MaxValue
    } else {
      availableTime max earliestStart
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    if (qtyResourceToConsume > availableCapacity)
      new ErrorResourceState(base)
    else {
      new ConsumableResourceState(base, startTime + taskDuration, availableCapacity - qtyResourceToConsume)
    }
  }
}

class ConsumableResourceWithSetupTimesState(base: ConsumableResourceWithSetupTimes,
                                            availableTime: Long,
                                            availableCapacity: Long,
                                            setupTimes: SetupTimes,
                                            currentMode: Int) extends ResourceState {

  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    if (qtyResourceToConsume > availableCapacity) {
      // Not available resource, activity will never start
      Long.MaxValue
    } else {
      // Check whether there is a mode change
      val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
      if (actMode == currentMode) {
        availableTime max earliestStart
      } else {
        val setupTime = setupTimes.mapSetupTimes.getOrElse((currentMode, actMode), 0L)
        (availableTime + setupTime) max earliestStart
      }
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val qtyResourceToConsume = base.mapConsumingActivities(activity)
    if (qtyResourceToConsume > availableCapacity)
      new ErrorResourceState(base)
    else {
      // Check whether there is a mode change
      val actMode = setupTimes.actModes.getOrElse(activity, currentMode)
      new ConsumableResourceWithSetupTimesState(base,
        startTime + taskDuration,
        availableCapacity - qtyResourceToConsume,
        setupTimes,
        actMode)
    }
  }
}

class ErrorResourceState(base: ResourceConstraint) extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = Long.MaxValue

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = this
}

// Case class that models Setup Times
case class SetupTimes(initialMode: Int, actModes: Map[Int, Int], mapSetupTimes: Map[(Int, Int), Long])