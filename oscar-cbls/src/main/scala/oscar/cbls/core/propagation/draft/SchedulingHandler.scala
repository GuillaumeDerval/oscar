package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

trait AbstractSchedulingHandler{
  def scheduleSHForPropagation(sh:SchedulingHandler)
}


class SchedulingHandler extends AbstractSchedulingHandler {
  //We use private[this] for faster field access by internal methods.
  private[this] var listeningSchedulingHandlers:QList[SchedulingHandler] = null
  private[this] var myRunner:Runner = null

  private[this] var isScheduled:Boolean = false
  private[this] var isRunning:Boolean = false

  private[this] var scheduledElements:QList[PropagationElement] = null
  private[this] var scheduledSHChildren:QList[SchedulingHandler] = null

  def runner_=(runner:Runner){
    myRunner = runner
  }
  def runner:Runner = myRunner

  def addListeningSchedulingHandler(sh:SchedulingHandler){
    listeningSchedulingHandlers = QList(sh,listeningSchedulingHandlers)
  }

  def schedulePEForPropagation(pe:PropagationElement): Unit ={
    if(isRunning){
      //we are actually propagating
      runner.enqueuePE(pe)
    }else {
      scheduledElements = QList(pe, scheduledElements)
      scheduleMyselfForPropagation()
    }
  }

  def scheduleSHForPropagation(sh:SchedulingHandler){
    this.synchronized {
      scheduledSHChildren = QList(sh, scheduledSHChildren)
      if (isRunning) {
        //We are actually propagating
        sh.enqueueForRun()
      } else {
        scheduleMyselfForPropagation()
      }
    }
  }

  private def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      isScheduled = true
      var listeningSchedulingHandlersAcc = listeningSchedulingHandlers
      while(listeningSchedulingHandlersAcc != null){
        listeningSchedulingHandlersAcc.head.scheduleSHForPropagation(this)
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }

  def enqueueForRun() {
    //We need to synchronize because some enqueue might be called following a propagation, which is performed multi-threaded
    require(this.isScheduled)
    require(!isRunning)
    this.synchronized {
      isRunning = true
      myRunner.enqueue(scheduledElements)
      scheduledElements = null
      var toScheduleSHChildren = scheduledSHChildren
      while(toScheduleSHChildren != null){
        toScheduleSHChildren.head.enqueueForRun()
        toScheduleSHChildren = toScheduleSHChildren.tail
      }
    }
  }

  def notifyEndRun(){
    if(isScheduled) {
      require(isRunning)
      require(scheduledElements == null)
      isScheduled = false
      isRunning = false
      while (scheduledSHChildren != null) {
        scheduledSHChildren.head.notifyEndRun()
        scheduledSHChildren = scheduledSHChildren.tail
      }
    }
  }
}

//the one for dynamic dependencies
class VaryingSchedulingHandler() extends SchedulingHandler{
  s.registerSchedulingHandler(this)

}


