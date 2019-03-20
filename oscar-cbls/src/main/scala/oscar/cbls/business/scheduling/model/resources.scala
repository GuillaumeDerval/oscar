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
                         val activitiesConsumption: Map[Int, Long])
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    val rbTreeMap: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
    new CumulativeResourceState(this, rbTreeMap.insert(0, capacity))
  }
}

class CumulativeResourceWithSetupTimes(val capacity: Long,
                                       val activitiesConsumption: Map[Int, Long],
                                       setupTimes: SetupTimes)
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    val rbTreeMap: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
    new CumulativeResourceWithSetupTimesState(this, rbTreeMap.insert(0L, capacity), setupTimes, setupTimes.initialMode)
  }
}

class CumulativeMultiResourceWithSetupTimes(val capacity: Long,
                                            val activitiesConsumption: Map[Int, Long],
                                            setupTimes: SetupTimes)
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    val rbTreeMap: RedBlackTreeMap[(Int, Long)] = RedBlackTreeMap.empty
    new CumulativeMultiResourceWithSetupTimesState(this,
                                                   rbTreeMap.insert(0L, (setupTimes.initialMode, capacity)),
                                                   setupTimes,
                                                   setupTimes.initialMode)
  }
}

class ConsumableResource(capacity: Long,
                         val activitiesConsumption: Map[Int, Long]) extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = activitiesConsumption.keys

  override def initialState: ResourceState =
    new ConsumableResourceState(this, 0L, capacity)
}

class ConsumableResourceWithSetupTimes(capacity: Long,
                                       val activitiesConsumption: Map[Int, Long],
                                       setupTimes: SetupTimes)
  extends ResourceConstraint {
  override def usingActivities: Iterable[Int] = activitiesConsumption.keys

  override def initialState: ResourceState =
    new ConsumableResourceWithSetupTimesState(this, 0L, capacity, setupTimes, setupTimes.initialMode)
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
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      availableTime max earliestStart
    } else {
      val setupTime = setupTimes.setupTimes.getOrElse((currentMode, actMode), 0L)
      (availableTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    new DisjunctiveResourceWithSetupTimesState(base, startTime + taskDuration, setupTimes, actMode)
  }
}

class CumulativeResourceState(base: CumulativeResource,
                              releaseTimes: RedBlackTreeMap[Long])
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    // Explore the RB Tree to obtain the release time of enough resources
    var rbExplorerOpt = releaseTimes.smallestPosition
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
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    // Explore the RB Tree to have the keys to delete
    var rbExplorerOpt = releaseTimes.smallestPosition
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
    var updatedReleaseTimes = releaseTimes
    keysToDelete.foreach { key =>
      updatedReleaseTimes = updatedReleaseTimes.remove(key)
    }
    // Add the key for last time if quantity was overloaded
    if (releasedQty > qtyResourceConsumed) {
      updatedReleaseTimes = updatedReleaseTimes.insert(releasedTime, releasedQty-qtyResourceConsumed)
    }
    // Add the key for the end time of the activity
    updatedReleaseTimes = updatedReleaseTimes.insert(startTime + taskDuration, qtyResourceConsumed)
    // Return the new state
    new CumulativeResourceState(base, updatedReleaseTimes)
  }
}

//TODO: this assumes that all resource in the cumulative resource have the same state.
//TODO: this model seems a bit odd in view of the reality.
//TODO: the correct model would be based on a more complex state model: timeToReleases:time -> ResourceState -> Amount
class CumulativeResourceWithSetupTimesState(base: CumulativeResourceWithSetupTimes,
                                            releaseTimes: RedBlackTreeMap[Long],
                                            setupTimes: SetupTimes,
                                            currentMode: Int)
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    // Explore the RB Tree to obtain the release time of enough resources
    var rbExplorerOpt = releaseTimes.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceToConsume) {
      releasedTime = rbExplorerOpt.get.key
      releasedQty += rbExplorerOpt.get.value
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    // Check whether there is a mode change
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      releasedTime max earliestStart
    } else {
      val setupTime = setupTimes.setupTimes.getOrElse((currentMode, actMode), 0L)
      (releasedTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    // Check whether there is a mode change
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      // No mode change: explore the RB Tree to have the keys to delete
      var rbExplorerOpt = releaseTimes.smallestPosition
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
      var updatedReleaseTimes = releaseTimes
      keysToDelete.foreach { key =>
        updatedReleaseTimes = updatedReleaseTimes.remove(key)
      }
      // Add the key for last time if quantity was overloaded
      if (releasedQty > qtyResourceConsumed) {
        updatedReleaseTimes = updatedReleaseTimes.insert(releasedTime, releasedQty-qtyResourceConsumed)
      }
      // Add the key for the end time of the activity
      updatedReleaseTimes = updatedReleaseTimes.insert(startTime + taskDuration, qtyResourceConsumed)
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, updatedReleaseTimes, setupTimes, actMode)
    } else {
      // Mode has changed:
      val emptyReleaseTimes: RedBlackTreeMap[Long] = RedBlackTreeMap.empty
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, emptyReleaseTimes.insert(startTime + taskDuration, base.capacity), setupTimes, actMode)
    }
  }
}

class CumulativeMultiResourceWithSetupTimesState(base: CumulativeMultiResourceWithSetupTimes,
                                                 releaseTimes: RedBlackTreeMap[(Int, Long)],
                                                 setupTimes: SetupTimes,
                                                 currentMode: Int)
  extends ResourceState {
  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    val releaseTimesAfterChanging = releaseTimesAfterChangingMode(actMode)
    // The exploration is on the new mapping where keys take account of setup times
    var rbExplorerOpt = releaseTimesAfterChanging.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceToConsume) {
      releasedTime = rbExplorerOpt.get.key
      releasedQty += rbExplorerOpt.get.value._2
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    // Result
    releasedTime max earliestStart
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    val releaseTimesAfterChanging = releaseTimesAfterChangingMode(actMode)
    // The exploration is on the new mapping where keys take account of setup times
    var rbExplorerOpt = releaseTimesAfterChanging.smallestPosition
    var releasedTime = 0L
    var releasedQty = 0L
    var lastMode = -1
    var lastSetupTime = 0L
    var lastQty = 0L
    var keysToDelete: List[Long] = Nil
    while (rbExplorerOpt.isDefined && releasedQty < qtyResourceConsumed) {
      lastMode = rbExplorerOpt.get.value._1
      lastSetupTime = setupTimes.setupTimes.getOrElse((lastMode, actMode), 0L)
      releasedTime = rbExplorerOpt.get.key - lastSetupTime
      releasedQty += rbExplorerOpt.get.value._2
      lastQty = rbExplorerOpt.get.value._2
      keysToDelete ::= releasedTime
      rbExplorerOpt = rbExplorerOpt.get.next
    }
    // Delete the keys
    var updatedReleaseTimes = releaseTimes
    keysToDelete.foreach { key =>
      updatedReleaseTimes = updatedReleaseTimes.remove(key)
    }
    // Add the key for last time if quantity was overloaded
    if (releasedQty > qtyResourceConsumed) {
      updatedReleaseTimes = updatedReleaseTimes.insert(releasedTime, (lastMode, releasedQty - qtyResourceConsumed))
    }
    // Add the key for the end time of the activity
    updatedReleaseTimes = updatedReleaseTimes.insert(startTime + taskDuration, (actMode, qtyResourceConsumed))
    // Return the new state
    new CumulativeMultiResourceWithSetupTimesState(base, updatedReleaseTimes, setupTimes, actMode)
  }

  private def releaseTimesAfterChangingMode(newMode: Int): RedBlackTreeMap[(Int, Long)] = {
    var newRBMap: RedBlackTreeMap[(Int, Long)] = RedBlackTreeMap.empty
    // Loop on releaseTimes map to change the key (release time) with the key + setupTime
    for {releasedTime <- releaseTimes.keys} {
      val releasedTuple = releaseTimes.get(releasedTime).get
      val releasedMode = releasedTuple._1
      val releasedQty = releasedTuple._2
      val changeModeTime = setupTimes.setupTimes.getOrElse((releasedMode, newMode), 0L)
      newRBMap = newRBMap.insert(releasedTime + changeModeTime, (releasedMode, releasedQty))
    }
    newRBMap
  }
}

class ConsumableResourceState(base: ConsumableResource,
                              availableTime: Long,
                              availableCapacity: Long) extends ResourceState {

  override def earliestStartTime(activity: Int, earliestStart: Long): Long = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
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
    val qtyResourceToConsume = base.activitiesConsumption(activity)
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
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity) {
      // Not available resource, activity will never start
      Long.MaxValue
    } else {
      // Check whether there is a mode change
      val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
      if (actMode == currentMode) {
        availableTime max earliestStart
      } else {
        val setupTime = setupTimes.setupTimes.getOrElse((currentMode, actMode), 0L)
        (availableTime + setupTime) max earliestStart
      }
    }
  }

  override def nextState(activity: Int, taskDuration: Long, startTime: Long): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity)
      new ErrorResourceState(base)
    else {
      // Check whether there is a mode change
      val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
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
case class SetupTimes(initialMode: Int, activityModes: Map[Int, Int], setupTimes: Map[(Int, Int), Long])