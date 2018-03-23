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

trait DynamicDependency extends PropagationElement{
  override def finishInitialization(){
    super.finishInitialization()
    //create the VSH structure here
  }
}


abstract class PropagationElement(val notificationBehavior:PropagationImpactCharacteristics){

  var layer:Int
  var threadID:Int
  var isScheduled:Boolean = false
  var schedulingHandler:SchedulingHandler
  var model:PropagationStructure = null

  def finishInitialization(): Unit ={
    schedulingHandler = model
  }

  def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      schedulingHandler.schedulePEForPropagation(this)
      isScheduled = true
    }
  }

  def reScheduleIfScheduled(): Unit ={
    if(isScheduled){
      schedulingHandler.schedulePEForPropagation(this)
    }
  }

  def triggerPropagation(){
    model.triggerPropagation(this)
  }

  final def performPropagation() = {
    if(isScheduled) {
      isScheduled = false
      propagate()
    }
  }

  protected def propagate() = ???
}


object PropagationImpactCharacteristics extends Enumeration{
  type PropagationImpactCharacteristics = Value
  val NotificationOnPropagate, NotificationOnNotify, NotificationOnNotifyAndPropagate, BulkElement = Value

  //Variables: NotificationOnPropagate
  //invariants classiques NotificationOnNotifyAndPropagate, si pas de propagate, alors NotificationOnNotify
  //IntInvariants NotificationOnPropagate (comme les variables en  fait) mais NotificationOnNotifyAndPropagate si ils ont plus de sorties
  //events NotificationOnNotifyAndPropagate
  //bulk BulkElement
}


import PropagationImpactCharacteristics._

class PropagationStructure(nbThreadOPtDefaultMax:Option[Int]) extends SchedulingHandler{

  var allSchedulingHandlers:QList[SchedulingHandler] = null
  def registerSchedulingHandler(s:SchedulingHandler): Unit ={
    allSchedulingHandlers = QList(s,allSchedulingHandlers)
  }

  var allPropagationElements:QList[PropagationElement]
  var layerToPropagationElements:Array[QList[PropagationElement]] = null
  var layerToNbPropagationElements:Array[Int] = null

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
    instantiateVSH()
    sortNodesByLayer()
    //create runner and communicate it to everyone
    runner = Runner(nbLayer,nbThreadOPtDefaultMax)
    //perform the multi-threaded analysis

    partitionGraphIntoSchedulingHandlers()
    for(sh <- allSchedulingHandlers){
      sh.runner = runner
    }
    if(runner.nbThread!=1) {
     partitionGraphIntoThreads()
    }
  }


  def sortNodesByLayer(): Int ={

  }


  def partitionGraphIntoSchedulingHandlers(): Unit ={

    for(pe <- propagationElements){
      allPropagationElements
    }
  }

  private[this] var propagating = false
  def isPropagating: Boolean = propagating

  def triggerPropagation(upTo:PropagationElement): Unit ={
    if(!propagating) {
      propagating = true
      runner.performPropagation(upTo)
      propagating = false
    }
  }

  def partitionGraphIntoThreads(){
    //goal is that each PE gets a threadIT
    //each PE has its propagationImpactZone: a set of PE's that will be impacted when PE.propagate is called.
    //two PE on the same layer must have the same TreadID if there is an intersection in their propagationImpactZone

    var currentTheadIDForNodeMarking:Int = -2
    for(layer <- 0 until nbLayer){

      var currentPEID = 0
      val pePositionInQListToDecoration:Array[Int] = Array.fill(layerToNbPropagationElements(layer))(0)
      val maximalTreadIMarkingOfCurrentLayer = currentTheadIDForNodeMarking

      for(pe <- layerToPropagationElements(layer)){
        currentPEID += 1
        currentTheadIDForNodeMarking = currentTheadIDForNodeMarking -1

        if(pe.threadID > maximalTreadIMarkingOfCurrentLayer ) {
          //the node has not been marked yet while exploring this layer
          pePositionInQListToDecoration(currentPEID) = currentTheadIDForNodeMarking
          pe.threadID = currentTheadIDForNodeMarking
          val interferingIDs:QList[Int] = markPropagationImpactZoneAndReportConflictingThreadIDMarkings(pe,currentTheadIDForNodeMarking, maximalTreadIMarkingOfCurrentLayer)
          if(interferingIDs != null){
            val interferenceList = QList(currentTheadIDForNodeMarking,interferingIDs)
            val largestID = interferenceList.max
          }
        }else{
          //this node was already marked as interfering with something in this layer
          //we do perform the marking, with the ID already used for this node
          pePositionInQListToDecoration(currentPEID) = pe.threadID
          val interferingIDs:QList[Int] = markPropagationImpactZoneAndReportConflictingThreadIDMarkings(pe,currentTheadIDForNodeMarking, maximalTreadIMarkingOfCurrentLayer)
        }
      }


      def markPropagationImpactZoneAndReportConflictingThreadIDMarkings(pe:PropagationElement,
                                                                        currentTheadIDForNodeMarking:Int,
                                                                        maximalTreadIDForReportingConflict:Int): QList[Int] ={

        pe.notificationBehavior match{
          case NotificationOnPropagate =>
          case NotificationOnNotify =>
          case NotificationOnNotifyAndPropagate =>
          case BulkElement =>
        }
      }
      //next, we find out proper thread IDs for each node
      val threadIDs = Array.fill(nbPE)(-1)




    }//end for each layer
  }
}

