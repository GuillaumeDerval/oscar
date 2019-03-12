package oscar.cbls.business.scheduling.invariants

import oscar.cbls.CBLSIntVar
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.model.{Precedences, ResourceConstraint, ResourceState}
import oscar.cbls.core.computation.SeqUpdate
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.{ChangingSeqValue, Invariant, SeqNotificationTarget}

class StartTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Array[Long],
                 actPrecedences: Precedences,
                 resourceConstraints: Array[ResourceConstraint],
                 makeSpan: CBLSIntVar,
                 startTimes: Array[CBLSIntVar])
  extends Invariant with SeqNotificationTarget {
  // Invariant initialization
  registerStaticAndDynamicDependency(actPriorityList)
  //TODO maybe other dependencies might be added
  finishInitialization()
  // Set defining invariant
  makeSpan.setDefiningInvariant(this)
  for {st <- startTimes} st.setDefiningInvariant(this)
  // Compute resources used by tasks
  val actUsedResources: Array[List[Int]] = Array.tabulate(actDurations.length)(_ => Nil)
  for {rcInd <- resourceConstraints.indices} {
    resourceConstraints(rcInd).usingActivities.foreach { actInd =>
      actUsedResources(actInd) ::= rcInd
    }
  }
  // Compute first start times
  computeStartTimes(actPriorityList.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    //TODO Incremental computation
    scheduleForPropagation()
  }

  //TODO recompute when another dimension changes
  override def performInvariantPropagation(): Unit = {
    computeStartTimes(actPriorityList.value)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    //TODO Implement this
  }

  // Compute the start times
  def computeStartTimes(actPriorityList: IntSequence): Unit = {
    val resourceStates: Array[ResourceState] = resourceConstraints.map(_.initialState)
    var makeSpanValue = 0L
    for {actInd <- actPriorityList} {
      val actIndI = actInd.toInt
      // Compute maximum ending time for preceding activities
      val maxEndTimePrecs = actPrecedences
        .precArray(actIndI)
        .filter(actPriorityList.contains(_))
        .foldLeft(0L) { (acc, precInd) =>
        acc max (startTimes(precInd).value + actDurations(precInd))
      }
      // Compute maximum of earliest release time for all needed resources
      val maxReleaseResources = actUsedResources(actIndI).foldLeft(0L) { (acc, resInd) =>
        acc max resourceStates(resInd).earliestStartTime(actIndI, 0L)
      }
      val earliestStartTime = maxEndTimePrecs max maxReleaseResources
      // Update resource states
      actUsedResources(actIndI).foreach { resInd =>
        resourceStates(resInd) = resourceStates(resInd).nextState(actIndI,
                                                                  actDurations(actIndI),
                                                                  earliestStartTime)
      }
      val actEndTime = earliestStartTime + actDurations(actIndI)
      if (actEndTime > makeSpanValue) {
        makeSpanValue = actEndTime
    }
      startTimes(actIndI) := earliestStartTime
    }
    makeSpan := makeSpanValue
  }
}

object StartTimes {
  def apply(actPriorityList: ChangingSeqValue,
            actDurations: Array[Long],
            actPrecedences: Precedences,
            resourceConstraints: Array[ResourceConstraint]): (CBLSIntVar, Array[CBLSIntVar]) = {
    val model = actPriorityList.model
    val makeSpan = CBLSIntVar(model, 0L, name="Schedule Makespan")
    val startTimes: Array[CBLSIntVar] = Array.tabulate(actDurations.length) { ind =>
      CBLSIntVar(model, 0L, name=s"Start Time of Activity ($ind)")
    }
    new StartTimes(actPriorityList, actDurations, actPrecedences, resourceConstraints, makeSpan, startTimes)
    (makeSpan, startTimes)
  }
}
