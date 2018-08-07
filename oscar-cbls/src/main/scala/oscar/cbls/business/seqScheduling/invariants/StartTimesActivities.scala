package oscar.cbls.business.seqScheduling.invariants

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.seqScheduling.model.SchedulingModel__A

object StartTimesActivities {
  def apply(priorityActivitiesList: ChangingSeqValue,
            schedulingModel: SchedulingModel__A): (Array[CBLSIntVar], SetupTimes) = {
    val model = priorityActivitiesList.model
    val startTimes: Array[CBLSIntVar] = Array.tabulate(schedulingModel.nbActivities)(node =>  CBLSIntVar(model, 0, name=s"Start Time of Activity $node"))
    val setupTimes = new SetupTimes

    new StartTimesActivities(priorityActivitiesList, schedulingModel, startTimes, setupTimes)

    (startTimes, setupTimes)
  }
}

class StartTimesActivities(priorityActivitiesList: ChangingSeqValue,
                           schedulingModel: SchedulingModel__A,
                           startTimes: Array[CBLSIntVar],
                           setupTimes: SetupTimes
                          ) extends Invariant with SeqNotificationTarget{

  // Invariant Initialization
  registerStaticAndDynamicDependency(priorityActivitiesList)

  finishInitialization()

  for {st <- startTimes} st.setDefiningInvariant(this)

  computeAllFromScratch(priorityActivitiesList.value)

  def computeAllFromScratch(priorityList: IntSequence): Unit = {
    // Initialization
    startTimes.foreach { stVar => stVar := 0}
    setupTimes.reset()
    val resourceFlowStates: Array[ResourceFlowState] = Array.tabulate(schedulingModel.nbResources) { i =>
      new ResourceFlowState(schedulingModel.resources(i).initialModeIndex,
                            schedulingModel.resources(i).capacity)
    }
    // Main loop
    for {indAct <- priorityList} {
      val resIndexesActI = schedulingModel.activities(indAct).resourceUsages.flatten.map(_.resourceIndex)
      val precedencesActI = schedulingModel.precedences.precArray(indAct)
      // compute maximum of start+duration for preceding activities
      val maxEndTimePrecsActI = if (precedencesActI.isEmpty) 0
        else precedencesActI.map { precInd =>
          startTimes(precInd).value + schedulingModel.activities(precInd).duration
        }.max
      // compute maximum of starting times for availability of resources
      var maxStartTimeAvailableResActI = 0
      for {resInd <- resIndexesActI} {
        // update last activity index
        resourceFlowStates(resInd).lastActivityIndex = indAct
        val lastModeRes = resourceFlowStates(resInd).lastRunningMode
        val newModeRes = schedulingModel.activities(indAct).resourceUsages(resInd).get.runningMode.index
        val lastResFlows = resourceFlowStates(resInd).forwardFlows
        val resourceQty = schedulingModel.activities(indAct).resourceUsages(resInd).get.capacity
        if (lastModeRes == newModeRes) {
          // running mode for resource in activity has not changed
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
            ResourceFlow.getEndTimeResourceFlows(resourceQty, lastResFlows))
        } else {
          // running mode for resource in activity changed
          resourceFlowStates(resInd).changedRunningMode = Some(lastModeRes)
          resourceFlowStates(resInd).lastRunningMode = newModeRes
          val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
          val setupTimeMode = schedulingModel.resources(resInd)
                                             .setupTimeModes(lastModeRes)(newModeRes)
                                             .get
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
            lastEndTime+setupTimeMode)
          // a new setup activity must be added
          setupTimes.addSetupTime(lastModeRes, newModeRes, setupTimeMode)
        }
      }
      // now we can update the start time for the activity
      startTimes(indAct) := math.max(maxEndTimePrecsActI, maxStartTimeAvailableResActI)
      // update map of resource flows for all resources. It requires another
      // loop on the resources
      for {resInd <- resIndexesActI} {
        val lastResFlows = resourceFlowStates(resInd).forwardFlows
        val resourceQty = schedulingModel.activities(indAct).resourceUsages(resInd).get.capacity
        resourceFlowStates(resInd).forwardFlows = ResourceFlow.addResourceFlowToList(
          indAct,
          resourceQty,
          schedulingModel.activities(indAct).duration,
          startTimes,
          ResourceFlow.flowQuantityResource(
            resourceQty,
            if (resourceFlowStates(resInd).changedRunningMode.isDefined) {
              // running mode has changed
              val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
              val setupTimeMode = schedulingModel.resources(resInd)
                                                 .setupTimeModes(resourceFlowStates(resInd).changedRunningMode.get)(resourceFlowStates(resInd).lastRunningMode)
                                                 .get
              List(SetupFlow(lastEndTime,
                             setupTimeMode,
                             schedulingModel.activities(indAct).resourceUsages(resInd).get.capacity))
            } else {
              lastResFlows
            }
          )
        )
      }
    }

  }

  //TODO Incremental computation
  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    computeAllFromScratch(changes.newValue)
  }

  /**
    * Internal class that carries the state of a resource flow
    */
  private class ResourceFlowState(initialModeInd: Int, maxCapacity: Int) {
    // Last activity that used this resource
    var lastActivityIndex: Int = -1
    // Forward flows after last activity that used this resource
    var forwardFlows: List[ResourceFlow] = List(SourceFlow(maxCapacity))
    // Running mode of last activity that used this resource
    var lastRunningMode: Int = initialModeInd
    // Checking whether running mode has changed
    var changedRunningMode: Option[Int] = None
  }

}

/**
  * This is a container class for setup times
  */
class SetupTimes {
  var setupTimesList: List[(Int, Int, Int)] = List()

  def reset(): Unit = {
    setupTimesList = List()
  }

  def addSetupTime(m0: Int, m1: Int, time: Int): Unit = {
    setupTimesList ::= (m0, m1, time)
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    for {st <- setupTimesList} {
      stringBuilder.append(s"Setup Time: ${st._1} -> ${st._2} = ${st._3}")
    }
    stringBuilder.toString
  }
}
