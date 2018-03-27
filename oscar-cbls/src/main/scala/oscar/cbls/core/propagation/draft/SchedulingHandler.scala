package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList
import oscar.cbls.core.propagation.draft.PropagationImpactCharacteristics.PropagationImpactCharacteristics

abstract class SchedulingHandler {

  var scheduledElements:QList[PropagationElement] = null
  var scheduledSHChildren:QList[SchedulingHandler] = null
  var isScheduled:Boolean = false
  var listeningSchedulingHandlers:QList[SchedulingHandler] = null
  private[this] var myRunner:Runner = null

  def scheduleSHForPropagation(sh:SchedulingHandler){
    scheduledSHChildren = QList(sh,scheduledSHChildren)
    scheduleMyselfForPropagation()
  }

  def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      isScheduled = true
      var listeningSchedulingHandlersAcc = listeningSchedulingHandlers
      while(listeningSchedulingHandlersAcc != null){
        listeningSchedulingHandlersAcc.head.scheduleSHForPropagation(this)
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }

  def schedulePEForPropagation(pe:PropagationElement): Unit ={
    scheduledElements = QList(pe,scheduledElements)
    scheduleMyselfForPropagation()
  }

  def runner_=(runner:Runner){
    myRunner = runner
  }
  def runner:Runner = myRunner

  def enqueueForRun() {
    //We need to synchronize because some enqueue might be called following a propagation, which is performed multi-threaded
    this.synchronized {
      if (isScheduled) {
        isScheduled = false
        myRunner.enqueue(scheduledElements)
        scheduledElements = null
        while(scheduledSHChildren != null){
          scheduledSHChildren.head.enqueueForRun()
          scheduledSHChildren = scheduledSHChildren.tail
        }
      }
    }
  }
}

class RegularSchedulingHandler(root:PropagationElement, s:PropagationStructure) extends SchedulingHandler{
  s.registerSchedulingHandler(this)


}

//the one for dynamic dependencies
class VaryingSchedulingHandler(s:PropagationStructure) extends SchedulingHandler{
  s.registerSchedulingHandler(this)


}



object PropagationImpactCharacteristics extends Enumeration{
  type PropagationImpactCharacteristics = Value
  val NoPropagationNotificationReceivedNoNotificationEmitted,
  NotificationOnPropagateNoNotificationReceived,
  BulkElement,
  NotificationOnNotifyNoPropagate,
  NotificationOnNotifyAndPropagate,
  NotificationOnPropagateReceivesNotification
  = Value

  //Variables: NotificationOnPropagate
  //invariants classiques NotificationOnNotifyAndPropagate, si pas de propagate, alors NotificationOnNotify
  //IntInvariants NotificationOnPropagate (comme les variables en  fait) mais NotificationOnNotifyAndPropagate si ils ont plus de sorties
  //events NotificationOnNotifyAndPropagate
  //bulk BulkElement
}


