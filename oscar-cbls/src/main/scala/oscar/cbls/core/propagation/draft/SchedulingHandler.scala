package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

import scala.collection.immutable.SortedSet

trait AbstractSchedulingHandler{
  def scheduleSHForPropagation(sh:SchedulingHandler)
}


class SchedulingHandler() extends AbstractSchedulingHandler {
  //We use private[this] for faster field access by internal methods.

  private[this] var myUniqueIDSH:Int = -1

  def uniqueIDSH_=(i:Int){
    require(myUniqueIDSH == -1)
    require(i != -1)
    myUniqueIDSH = i
  }

  def uniqueIDSH:Int = myUniqueIDSH

  private[this] var listeningSchedulingHandlers:QList[SchedulingHandler] = null
  private[this] var myRunner:Runner = null

  var isScheduled:Boolean = false
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
class VaryingSchedulingHandler(val p:PropagationElement, s:PropagationStructure) extends SchedulingHandler{
  s.registerSchedulingHandler(this)

  var isDeterminingElementScheduled:Boolean


  //when this one is registered for run, it schedules the trigger for propagation
  //when it is enqueed for run, it enqueues the SHof the determining element
  //when the trigger is propagated, it calls this class to

  def runNeededDynamicDependencies(): Unit ={
    for(dynamicallyListenedPE <- p.dynamicallyListenedElements if dynamicallyListenedPE != p.determiningElement){
      if(dynamicallyListenedPE.schedulingHandler.isScheduled){
        dynamicallyListenedPE.schedulingHandler.enqueueForRun()
      }
    }
  }

  def runDeterminingElementIfNeeded():Boolean={
    if(p.determiningElement == null){
      false
    }else{

    }

  }
}

class VaryingSchedulingHandlerTrigger(vsh:VaryingSchedulingHandler) extends PropagationElement(){

  //We register this to be between the listened elements and the varying dependency PE
  for(listenedStaticElement <- vsh.p.staticallyListenedElements){
    listenedStaticElement.registerStaticallyListeningElement(this)
  }
  this.registerStaticallyListeningElement(vsh.p)

  override protected def performPropagation(): Unit ={
    vsh.runNeededDynamicDependencies()
  }
}


class PropagationStructurePartitionner(p:PropagationStructure){

  def instantiateVariableSchedulingHandlers(): Unit ={

  }

  //TODO how about dynamic dependencies?
  //TODO how about SCC?
  //TODO how about statically listening that are in a SCC?

  //graph must already be sorted by layers
  def partitionIntoSchedulingHandlers(){
    var currentLayerID = p.layerToClusteredPropagationElements.length
    while(currentLayerID >0){
      currentLayerID = currentLayerID -1

      //from outputs to inputs decorate layer by layer
      //the SH of a node is the SH of its successors
      //except if the node already has a sh or if its successors have different sh's

      for(pe <- p.layerToClusteredPropagationElements(currentLayerID)){

        if (pe.schedulingHandler == p) {
          //it needs to be decorated

          var staticallyListeningElements = pe.staticallyListeningElements

          if (staticallyListeningElements == null) {
            //it has no succesor, so it gets a new scheduling handler and job is done.

            val newSchedulingHandler = new SchedulingHandler()
            p.registerSchedulingHandler(newSchedulingHandler)
            pe.schedulingHandler = newSchedulingHandler

          } else {
            val referenceListeningSchedulingHandler = staticallyListeningElements.head.schedulingHandler
            staticallyListeningElements = staticallyListeningElements.tail
            while (staticallyListeningElements != null) {
              if (staticallyListeningElements.head.schedulingHandler != referenceListeningSchedulingHandler) {
                //there are more than one listening scheduling handler, so we create a scheduling handler on pe
                val newSchedulingHandler = new SchedulingHandler()
                p.registerSchedulingHandler(newSchedulingHandler)
                pe.schedulingHandler = newSchedulingHandler

                newSchedulingHandler.addListeningSchedulingHandler(referenceListeningSchedulingHandler)
                newSchedulingHandler.addListeningSchedulingHandler(staticallyListeningElements.head.schedulingHandler)

                var knownIDs: SortedSet[Int] = SortedSet(referenceListeningSchedulingHandler.uniqueIDSH,
                  staticallyListeningElements.head.schedulingHandler.uniqueIDSH)

                while (staticallyListeningElements != null) {
                  val sh = staticallyListeningElements.head.schedulingHandler
                  staticallyListeningElements = staticallyListeningElements.tail

                  if (!(knownIDs contains sh.uniqueIDSH)) {
                    knownIDs = knownIDs + sh.uniqueIDSH
                    newSchedulingHandler.addListeningSchedulingHandler(sh)
                  }
                }
              } else {
                staticallyListeningElements = staticallyListeningElements.tail
              }
            }

          }
        }else{
          //it has already a scheduling handler, so we add all listening scheduling handler to this one
          var staticallyListeningElements = pe.staticallyListeningElements
          var knownIDs: SortedSet[Int] = SortedSet.empty

          while (staticallyListeningElements != null) {
            val sh = staticallyListeningElements.head.schedulingHandler
            staticallyListeningElements = staticallyListeningElements.tail

            if (!(knownIDs contains sh.uniqueIDSH)) {
              knownIDs = knownIDs + sh.uniqueIDSH
              pe.schedulingHandler.addListeningSchedulingHandler(sh)
            }
          }
        }
      }

    }
  }
}
