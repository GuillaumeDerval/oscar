package oscar.cbls.business.seqScheduling.model

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
                            resourceFlows: List[ResourceFlow]): List[ResourceFlow] = {
    // Auxiliary Function
    def addResourceFlowToList(resFlow: ResourceFlow,
                              resourceFlows: List[ResourceFlow],
                              yetAdded: Boolean,
                              accFlows: List[ResourceFlow]): List[ResourceFlow] = {
      resourceFlows match {
        case Nil =>
          if (yetAdded) accFlows else resFlow::accFlows
        case rfl::rfls =>
          if (yetAdded) {
            addResourceFlowToList(resFlow, rfls, yetAdded, rfl::accFlows)
          } else if (resFlow.endTime < rfl.endTime) {
            addResourceFlowToList(resFlow, rfls, true, resFlow::rfl::accFlows)
          } else {
            addResourceFlowToList(resFlow, rfls, yetAdded, rfl::accFlows)
          }
      }
    }
    /////
    val flowToInsert = ActivityFlow(indexAct, startActs(indexAct), durAct, qtyAct)
    addResourceFlowToList(flowToInsert, resourceFlows, false, Nil).reverse
  }

  /**
    * Consumes a quantity of resource in a flow list
    *
    * @param qty the quantity of resource to be consumed in the flow list
    * @param resourceFlows a list of resource flows
    * @return a new list of resource flows with qty consumed.
    */
  def flowQuantityResource(qty: Int,
                           resourceFlows: List[ResourceFlow]): List[ResourceFlow] = {
    if (qty == 0)
      resourceFlows
    else {
      resourceFlows match {
        case Nil => Nil
        case rfl::rfls =>
          if (qty < rfl.quantity) rfl.changedQuantity(rfl.quantity-qty)::rfls
          else flowQuantityResource(qty-rfl.quantity, rfls)
      }
    }
  }

  /**
    * Consumes a quantity of a resource and gives a flow list
    *
    * @param qty the quantity of resource to be consumed
    * @param resourceFlow the resource
    * @return a resource flow list containing the resource flow without the quantity
    */
  def flowQuantityResource(qty: Int,
                           resourceFlow: ResourceFlow): List[ResourceFlow] = {
    if (qty == resourceFlow.quantity)
      Nil
    else
      List(resourceFlow.changedQuantity(resourceFlow.quantity-qty))
  }

  /**
    * Gets the latest end time for consuming a quantity in some resource flows
    *
    * @param resQty the quantity of resource that must be consumed
    * @param resFlows the list of resource flows
    * @return the end time of the activity allowing to consume all the resQty
    *         of resource in resFlows
    */
  def getEndTimeResourceFlows(resQty: Int, resFlows: List[ResourceFlow]): Int = {
    resFlows match {
      case Nil => 0
      case rfl::rfls =>
        if (resQty > rfl.quantity)
          getEndTimeResourceFlows(resQty-rfl.quantity, rfls)
        else
          rfl.endTime
    }
  }

  /**
    * Gets the latest end time of a list of resource flows
    *
    * @param resFlows the list of resource flows
    * @return the endtime of the last resource flow in the list
    */
  def getLastEndTimeResourceFlow(resFlows: List[ResourceFlow]): Int = {
    if (resFlows.isEmpty) 0
    else resFlows.last.endTime
  }
}