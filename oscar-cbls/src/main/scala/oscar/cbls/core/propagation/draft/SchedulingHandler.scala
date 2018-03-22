package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

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

trait DynamicDependency extends PropagationElement{
  override def finishInitialization(){
    super.finishInitialization()
    //create the VSH structure here
  }
}


class PropagationElement{

  var layer:Int
  var threadID:Int
  var isScheduled:Boolean
  var schedulingHandler:SchedulingHandler

  def finishInitialization(): Unit ={
    schedulingHandler = model
  }

  def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      schedulingHandler.schedulePEForPropagation(this)
      isScheduled = true
    }
  }

  def performPropagation() = {
    if(isScheduled) {
      isScheduled = false
      propagate()
    }
  }

  def propagate() = ???
}

class PropagationStructure(nbThreadOPtDefaultMax:Option[Int]) extends SchedulingHandler{

  var allSchedulingHandlers:QList[SchedulingHandler] = null
  def registerSchedulingHandler(s:SchedulingHandler): Unit ={
    allSchedulingHandlers = QList(s,allSchedulingHandlers)
  }

  var allPropagationElements:QList[PropagationElement]
  var layerToAllPropagationElements:Array[QList[PropagationElement]] = null

  //can only be called when all SH are created
  override def runner_=(runner: Runner){
    super.runner_=(runner)
    for(s <- allSchedulingHandlers){
      s.runner = runner
    }
  }


  def registerForPartialPropagation(pe:PropagationElement): Unit ={
    //we root a schedulingHandler at this node.
  }

  private var nbLayer:Int = -1
  private var runner:Runner = null


  def close(): Unit ={
    identifySCC()
    sortNodesByLayer()
    runner = Runner(nbLayer,nbThreadOPtDefaultMax)
    partitionGraphIntoSchedulingHandlers()
    for(sh <- allSchedulingHandlers){
      sh.runner = runner
    }
  }


  def sortNodesByLayer(): Int ={

  }


  def partitionGraphIntoSchedulingHandlers(): Unit ={

    for(pe <- propagationElements){
      allPropagationElements
    }
  }
}