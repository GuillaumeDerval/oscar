package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

trait SchedulingHandler{
  def scheduleSHForPropagation(sh:SchedulingHandler,isStillValid:()=>Boolean)
  def schedulePEForPropagation(pe:PropagationElement): Unit
  def isSCC:Boolean
  def loadScheduledElementsAndAllSourcesIntoRunner()
  def notifyEndRun()
}

class SimpleSchedulingHandler() extends SchedulingHandler {
  //We use private[this] for faster field access by internal methods.

  override def isSCC: Boolean = false

  private[this] var myUniqueIDSH:Int = -1

  def uniqueIDSH_=(i:Int){
    require(myUniqueIDSH == -1)
    require(i != -1)
    myUniqueIDSH = i
  }

  def uniqueIDSH:Int = myUniqueIDSH

  private[this] var listeningSchedulingHandlers:QList[SchedulingHandler] = null
  protected [this] var myRunner:Runner = null

  var isScheduled:Boolean = false
  protected[this] var isRunning:Boolean = false

  protected [this] var scheduledElements:QList[PropagationElement] = null
  class ScheduledSHAndValidityTest(val sh:SchedulingHandler,val isStillValid:()=>Boolean)
  private[this] var scheduledSHChildren:QList[ScheduledSHAndValidityTest] = null

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

  def scheduleSHForPropagation(sh:SchedulingHandler,isStillValid:()=>Boolean){
      scheduledSHChildren = QList(new ScheduledSHAndValidityTest(sh,isStillValid), scheduledSHChildren)
      if (isRunning) {
        //We are actually propagating
        sh.loadScheduledElementsAndAllSourcesIntoRunner()
      } else {
        scheduleMyselfForPropagation()
      }
    }

  protected def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      isScheduled = true
      var listeningSchedulingHandlersAcc = listeningSchedulingHandlers
      while(listeningSchedulingHandlersAcc != null){
        listeningSchedulingHandlersAcc.head.scheduleSHForPropagation(this,null)
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }

  override def loadScheduledElementsAndAllSourcesIntoRunner() {
    if (isScheduled && !isRunning) {
      //could be not scheduled anymore since SH might be listened by several SH
      //could already be running, for the same reason.

      isRunning = true
      myRunner.enqueue(scheduledElements)
      scheduledElements = null
      var toScheduleSHChildren = scheduledSHChildren
      scheduledSHChildren = null
      while (toScheduleSHChildren != null) {
        val enqueue = toScheduleSHChildren.head
        if(enqueue.isStillValid == null || enqueue.isStillValid()){
          enqueue.sh.loadScheduledElementsAndAllSourcesIntoRunner()
          scheduledSHChildren = QList(enqueue,scheduledSHChildren)
        }
        toScheduleSHChildren = toScheduleSHChildren.tail
      }
    }
  }

  override def notifyEndRun(){
    if(isScheduled) {
      require(isRunning)
      require(scheduledElements == null)
      isScheduled = false
      isRunning = false
      while (scheduledSHChildren != null) {
        scheduledSHChildren.head.sh.notifyEndRun()
        scheduledSHChildren = scheduledSHChildren.tail
      }
    }
  }
}
