package oscar.cbls.business.seqScheduling.invariants

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.model._

object StartTimesActivities {
  def apply(priorityActivitiesList: ChangingSeqValue,
            schedulingProblem: SchedulingProblem): (CBLSIntVar, Array[CBLSIntVar], SetupTimes) = {
    val model = priorityActivitiesList.model
    val makeSpan = CBLSIntVar(model, 0, name="Schedule Makespan")
    val startTimes: Array[CBLSIntVar] = Array.tabulate(schedulingProblem.activities.size)(node =>  CBLSIntVar(model, 0, name=s"Start Time of Activity ${schedulingProblem.activities.elementAt(node).name}"))
    val setupTimes = new SetupTimes

    new StartTimesActivities(priorityActivitiesList, schedulingProblem, makeSpan, startTimes, setupTimes)

    (makeSpan, startTimes, setupTimes)
  }
}

/**
  * Main invariant of a scheduling problem: the start time of the activities
  *
  * @param priorityActivitiesList a sequence of the activities indicating the order in
  *                               which the start time will be set (Pralet algorithm)
  * @param schP the scheduling problem
  * @param makeSpan the makeSpan (output)
  * @param startTimes the start times for each activity (output)
  * @param setupTimes the setup times (output)
  */
class StartTimesActivities(priorityActivitiesList: ChangingSeqValue,
                           schP: SchedulingProblem,
                           makeSpan: CBLSIntVar,
                           startTimes: Array[CBLSIntVar],
                           setupTimes: SetupTimes
                          ) extends Invariant with SeqNotificationTarget {

  // Invariant Initialization
  registerStaticAndDynamicDependency(priorityActivitiesList)
  schP.activities.toIterable.foreach(act =>
    registerStaticAndDynamicDependency(act.durationCBLS)
  )
  schP.resources.toIterable.foreach(res =>
    registerStaticAndDynamicDependency(res.capacityCBLS)
  )

  finishInitialization()

  for {st <- startTimes} st.setDefiningInvariant(this)
  makeSpan.setDefiningInvariant(this)

  computeAllFromScratch(priorityActivitiesList.value)

  /**
    * Main algorithm to compute the start times, makespan and setup times
    * for changing modes in the scheduling
    *
    * @param priorityList a sequence of activity indices, which indicates the
    *                     order in which the start times will be set
    */
  def computeAllFromScratch(priorityList: IntSequence): Unit = {
    // Initialization
    setupTimes.reset()
    val resourceFlowStates: Array[ResourceFlowState] = Array.tabulate(schP.resources.size) { i =>
      new ResourceFlowState(schP.resources.elementAt(i).runningModes.initialModeIndex,
                            schP.resources.elementAt(i).capacity)
    }
    var makeSpanValue = 0
    val startTimesArray: Array[Int] = Array.tabulate(schP.activities.size)(_ => 0)
    // Main loop
    for {indAct <- priorityList} {
      val resIndexesActI = schP
        .activityResourceUsages
        .resourceUsages(indAct)
        .indices
        .filter(indRes => schP.activityResourceUsages.resourceUsages(indAct)(indRes).isDefined)
      val precedencesActI = schP.precedences.precArray(indAct)
      // compute maximum of start+duration for preceding activities
      val maxEndTimePrecsActI = if (precedencesActI.isEmpty) 0
        else precedencesActI.map(precInd =>
          startTimes(precInd).value + schP.activities.elementAt(precInd).valDuration
        ).max
      // compute maximum of starting times for availability of resources
      var maxStartTimeAvailableResActI = 0
      for {resInd <- resIndexesActI} {
        // update last activity index
        resourceFlowStates(resInd).lastActivityIndex = indAct
        val lastModeRes = resourceFlowStates(resInd).lastRunningMode
        val newModeRes = schP
          .activityResourceUsages
          .resourceUsages(indAct)(resInd)
          .get
          .indexRM
        val lastResFlows = resourceFlowStates(resInd).forwardFlows
        val resourceQty = schP
          .activityResourceUsages
          .resourceUsages(indAct)(resInd)
          .get
          .capacity
        if (lastModeRes == newModeRes) {
          // running mode for resource in activity has not changed
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
            ResourceFlow.getEndTimeResourceFlows(resourceQty, lastResFlows))
        } else {
          // running mode for resource in activity changed
          resourceFlowStates(resInd).changedRunningMode = Some(lastModeRes)
          resourceFlowStates(resInd).lastRunningMode = newModeRes
          val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
          val setupTimeMode = schP
            .resources
            .elementAt(resInd)
            .runningModes
            .setupTime(lastModeRes, newModeRes)
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
            lastEndTime + setupTimeMode)
          // a new setup activity must be added
          setupTimes.addSetupTime(SetupTimeData(resInd, lastModeRes, newModeRes, lastEndTime, setupTimeMode))
        }
      }
      // now we can update the start time for the activity and the makespan
      val startTimeAct = math.max(maxEndTimePrecsActI, maxStartTimeAvailableResActI)
      startTimesArray(indAct) = startTimeAct
      val endTimeAct = startTimeAct + schP.activities.elementAt(indAct).valDuration
      if (endTimeAct > makeSpanValue) {
        makeSpanValue = endTimeAct
      }
      // update map of resource flows for all resources. It requires another
      // loop on the resources
      for {resInd <- resIndexesActI} {
        val lastResFlows = resourceFlowStates(resInd).forwardFlows
        val resourceQty = schP
          .activityResourceUsages
          .resourceUsages(indAct)(resInd)
          .get
          .capacity
        resourceFlowStates(resInd).forwardFlows = ResourceFlow.addResourceFlowToList(
          indAct,
          resourceQty,
          schP.activities.elementAt(indAct).valDuration,
          startTimesArray,
          ResourceFlow.flowQuantityResource(
            resourceQty,
            if (resourceFlowStates(resInd).changedRunningMode.isDefined) {
              // running mode has changed
              val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
              val setupTimeMode = schP
                .resources
                .elementAt(resInd)
                .runningModes
                .setupTime(resourceFlowStates(resInd).changedRunningMode.get,
                  resourceFlowStates(resInd).lastRunningMode)
              List(SetupFlow(lastEndTime, setupTimeMode, resourceQty))
            } else {
              lastResFlows
            }
          )
        )
      }
    }
    // Set makespan variable
    makeSpan := makeSpanValue
    // Set start times variables
    for { i <- 0 until schP.activities.size} {
      startTimes(i) := startTimesArray(i)
    }
  }

  //TODO Incremental computation
  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    scheduleForPropagation()
  }

  //TODO: il faut également planifier un re-calcul quand une autre dimension change: durée des tâche, attribut e resoruces, etc. J'ai vu des variables dans des resources et tâches.

  override def performInvariantPropagation(): Unit = {
    computeAllFromScratch(priorityActivitiesList.value)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    //TODO: Implement check internals
  }

  /**
    * Internal class that carries the state of a resource flow in the scheduling
    */
  private class ResourceFlowState(initialModeInd: Int, maxCapacity: Int) {
    //TODO: je suis pas convaincu parce-que toutes les catégories de resources ont le même state du coup
    // Last activity that used this resource
    var lastActivityIndex: Int = -1
    // Forward flows after last activity that used this resource
    var forwardFlows: List[ResourceFlow] = List(SourceFlow(maxCapacity))
    // Running mode of last activity that used this resource
    var lastRunningMode: Int = initialModeInd
    // Checking whether running mode has changed
    var changedRunningMode: Option[Int] = None

    override def toString: String =
      s"""****
         |RFW:
         |Initial mode : $initialModeInd
         |Max capacity : $maxCapacity
         |Last mode : $lastRunningMode
         |Last activity : $lastActivityIndex
         |Forward flows : $forwardFlows
         |****
       """.stripMargin
  }
}

/**
  * This case class represents a setup time for changing a running mode in a resource
  */
case class SetupTimeData(resourceIndex: Int, modeFromInd: Int, modeToInd: Int, startTime: Int, duration: Int)
//TODO: je pense que les SetupTimeData devraient être dans la resource concernée

/**
  * This is a container class for setup times
  */
class SetupTimes {
  var setupTimesList: List[SetupTimeData] = List()

  def reset(): Unit = {
    setupTimesList = List()
  }

  def addSetupTime(st: SetupTimeData): Unit = {
    setupTimesList :+= st
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append("SetupTimes: \n")
    for {st <- setupTimesList} {
      stringBuilder.append(s"* $st \n")
    }
    stringBuilder.toString
  }
}
