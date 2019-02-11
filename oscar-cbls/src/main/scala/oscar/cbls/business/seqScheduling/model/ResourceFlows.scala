package oscar.cbls.business.seqScheduling.model

import oscar.cbls.algo.heap.{AbstractHeap, BinomialHeap}

/**
  * Abstract class representing the flow of a resource
  */
abstract class ResourceFlow {
  // quantity of consumed resource
  val quantity: Int
  // ending time in the schedule for this resource flow
  val endTime: Int

  /**
    * Resource flow after changing the quantity of resource
    *
    * @param newQty the new quantity of the resource
    * @return a new resource flow reflecting
    */
  def changedQuantity(newQty: Int): ResourceFlow
}

/**
  * Flow from the "source", representing the initial resource flows in the
  * schedule
  *
  * @param quantity the quantity of the resource
  */
case class SourceFlow(quantity: Int) extends ResourceFlow {
  val endTime: Int = 0

  override def changedQuantity(newQty: Int): ResourceFlow =
    SourceFlow(newQty)
}

/**
  * This class represents the flow of a resource through an activity
  *
  * @param activityIndex the index of the activity from which the resource flows
  * @param startTime the start time of the activity in the flow
  * @param duration the duration of the activity in the flow
  * @param quantity the quantity of the resource in the flow
  */
case class ActivityFlow(activityIndex: Int,
                        startTime: Int,
                        duration: Int,
                        quantity: Int) extends ResourceFlow {
  val endTime: Int = startTime + duration

  override def changedQuantity(newQty: Int): ResourceFlow =
    ActivityFlow(activityIndex, startTime, duration, newQty)
}

/**
  * This class represents the flow of a resource through a setup activity
  * (for changing running modes)
  *
  * @param startTime the start time of the flow
  * @param duration the duration of the setup
  * @param quantity the quantity of the resource
  */
case class SetupFlow(startTime: Int,
                     duration: Int,
                     quantity: Int) extends ResourceFlow {
  val endTime: Int = startTime + duration

  override def changedQuantity(newQty: Int): ResourceFlow =
    SetupFlow(startTime, duration, newQty)
}

/**
  * Companion object with some basic operations on Resource Flows
  */
object ResourceFlow {
  /**
    * Adds the resource flow for an activity to the resource flow list
    *
    * @param indexAct the index of the activity to add in the flow list
    * @param qtyAct the quantity of resource consumed by the activity
    * @param durAct the duration of the activities
    * @param startActs the start times of all activities
    * @param resourceFlows the resource flow list
    * @return a new resource flow list where the pair (indAct, qtyAct) has been
    *         inserted. The list is sorted by (startTime+duration)
    */
  def addResourceFlowToList(indexAct: Int,
                            qtyAct: Int,
                            durAct: Int,
                            startActs: Array[Int],
                            resourceFlows: AbstractHeap[ResourceFlow]): AbstractHeap[ResourceFlow] = {
    resourceFlows.insert(ActivityFlow(indexAct, startActs(indexAct), durAct, qtyAct))
    resourceFlows
  }

  /**
    * Consumes a quantity of resource in a flow list
    *
    * @param qty the quantity of resource to be consumed in the flow list
    * @param resourceFlows a list of resource flows
    * @return a new list of resource flows with qty consumed.
    */
  def flowQuantityResource(qty: Int,
                           maxCapacity: Int,
                           resourceFlows: AbstractHeap[ResourceFlow]): AbstractHeap[ResourceFlow] = {
    var qtyToConsume = qty
    val it = resourceFlows.iterator
    val result = new BinomialHeap[ResourceFlow](-_.endTime, maxCapacity)
    while (it.hasNext) {
      val resFlow = it.next()
      if (qtyToConsume > 0) {
        if (resFlow.quantity > qtyToConsume) {
          result.insert(resFlow.changedQuantity(resFlow.quantity - qtyToConsume))
        } else {
          qtyToConsume -= resFlow.quantity
        }
      } else {
        result.insert(resFlow)
      }
    }
    result
  }

  /**
    * Consumes a quantity of a resource and gives a flow list
    *
    * @param qty the quantity of resource to be consumed
    * @param resourceFlow the resource
    * @return a resource flow list containing the resource flow without the quantity
    */
  def flowQuantityResource(qty: Int,
                           resourceFlow: ResourceFlow,
                           lastResFlows: AbstractHeap[ResourceFlow]): AbstractHeap[ResourceFlow] = {
    lastResFlows.dropAll()
    if (resourceFlow.quantity > qty) {
      lastResFlows.insert(resourceFlow.changedQuantity(resourceFlow.quantity - qty))
    }
    lastResFlows
  }

  /**
    * Gets the latest end time for consuming a quantity in some resource flows
    *
    * @param resQty the quantity of resource that must be consumed
    * @param resFlows the list of resource flows
    * @return the end time of the activity allowing to consume all the resQty
    *         of resource in resFlows
    */
  def getEndTimeResourceFlows(resQty: Int, resFlows: AbstractHeap[ResourceFlow]): Int = {
    val it = resFlows.iterator
    var endTime = 0
    var qtyToConsume = resQty
    while (qtyToConsume > 0 && it.hasNext) {
      val resFlow = it.next()
      endTime = resFlow.endTime
      if (resFlow.quantity > qtyToConsume) {
        qtyToConsume = 0
      } else {
        qtyToConsume -= resFlow.quantity
      }
    }
    endTime
  }

  /**
    * Gets the latest end time of a list of resource flows
    *
    * @param resFlows the list of resource flows
    * @return the endtime of the last resource flow in the list
    */
  def getLastEndTimeResourceFlow(resFlows: AbstractHeap[ResourceFlow]): Int = {
    val it = resFlows.iterator
    var endTime = 0
    while (it.hasNext) {
      endTime = it.next().endTime
    }
    endTime
  }
}

/**
  * This class carries the state of a resource flow in the scheduling
  *
  * @param resourceIndex the index of the resource
  * @param initialModeInd the index of the initial mode of the resource
  * @param maxCapacity the maximum capacity of the resource
  */
class ResourceFlowState(val resourceIndex: Int, initialModeInd: Int, maxCapacity: Int) {
  // Last activity that used this resource
  var lastActivityIndex: Int = -1
  // Forward flows after last activity that used this resource
  var forwardFlows: AbstractHeap[ResourceFlow] = new BinomialHeap[ResourceFlow](-_.endTime, maxCapacity)
  forwardFlows.insert(SourceFlow(maxCapacity))
  // Running mode of last activity that used this resource
  var lastRunningMode: Int = initialModeInd
  // Checking whether running mode has changed
  var changedRunningMode: Option[Int] = None

  override def toString: String =
    s"""****
       |RFW:
       |Resource index : $resourceIndex
       |Initial mode : $initialModeInd
       |Max capacity : $maxCapacity
       |Last mode : $lastRunningMode
       |Last activity : $lastActivityIndex
       |Forward flows : $forwardFlows
       |****
       """.stripMargin
}