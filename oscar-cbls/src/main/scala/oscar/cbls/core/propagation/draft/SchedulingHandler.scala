package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

import scala.collection.immutable.SortedSet

trait AbstractSchedulingHandler{
  def scheduleSHForPropagation(sh:SimpleSchedulingHandler,isStillValid:()=>Boolean)
}

class SimpleSchedulingHandler() extends AbstractSchedulingHandler {
  //We use private[this] for faster field access by internal methods.

  private[this] var myUniqueIDSH:Int = -1

  def uniqueIDSH_=(i:Int){
    require(myUniqueIDSH == -1)
    require(i != -1)
    myUniqueIDSH = i
  }

  def uniqueIDSH:Int = myUniqueIDSH

  private[this] var listeningSchedulingHandlers:QList[SimpleSchedulingHandler] = null
  protected [this] var myRunner:Runner = null

  var isScheduled:Boolean = false
  protected[this] var isRunning:Boolean = false

  protected [this] var scheduledElements:QList[PropagationElement] = null
  class ScheduledSHAndValidityTest(val sh:SimpleSchedulingHandler,val isStillValid:()=>Boolean)
  private[this] var scheduledSHChildren:QList[ScheduledSHAndValidityTest] = null

  def runner_=(runner:Runner){
    myRunner = runner
  }
  def runner:Runner = myRunner

  def addListeningSchedulingHandler(sh:SimpleSchedulingHandler){
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

  def scheduleSHForPropagation(sh:SimpleSchedulingHandler,isStillValid:()=>Boolean){
    this.synchronized {
      scheduledSHChildren = QList(new ScheduledSHAndValidityTest(sh,isStillValid), scheduledSHChildren)
      if (isRunning) {
        //We are actually propagating
        sh.enqueueForRun()
      } else {
        scheduleMyselfForPropagation()
      }
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

  def enqueueForRun() {
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
          enqueue.sh.enqueueForRun()
          scheduledSHChildren = QList(enqueue,scheduledSHChildren)
        }
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
        scheduledSHChildren.head.sh.notifyEndRun()
        scheduledSHChildren = scheduledSHChildren.tail
      }
    }
  }
}

class RootedSchedulingHandlerDynListened(val root:PropagationElement) extends RootedSchedulingHandlerDynListenedTrait {
  //among all scheduled element with this scheduling handler,
  //only the root can be listened by some PE not in the SH
}

trait RootedSchedulingHandlerDynListenedTrait extends SimpleSchedulingHandler {
  val root:PropagationElement

  root.dynamicallyListeningElements.notifyInserts(notifyRootListenedByNewPE)

  class ListeningSHAndValidityTest(val sh:SimpleSchedulingHandler,val isStillValid:()=>Boolean)
  var dynamicallyListeningSHAndValidityTest:QList[ListeningSHAndValidityTest] = null

  def notifyRootListenedByNewPE(newListeningPE:(PropagationElement,Int),isStillValid:()=> Boolean) {
    if (isStillValid == null || isStillValid()) {
      dynamicallyListeningSHAndValidityTest = QList(new ListeningSHAndValidityTest(newListeningPE._1.schedulingHandler, isStillValid), dynamicallyListeningSHAndValidityTest)
      if (isScheduled) {
        newListeningPE._1.schedulingHandler.scheduleSHForPropagation(this, isStillValid)
      }
    }
  }

  override protected def scheduleMyselfForPropagation(){
    if(!isScheduled){
      isScheduled = true
      var listeningSchedulingHandlersAcc = dynamicallyListeningSHAndValidityTest
      dynamicallyListeningSHAndValidityTest = null
      while(listeningSchedulingHandlersAcc != null){
        val prop = listeningSchedulingHandlersAcc.head
        val isSTillValidFn = prop.isStillValid
        if(isSTillValidFn()) {
          prop.sh.scheduleSHForPropagation(this, prop.isStillValid)
          dynamicallyListeningSHAndValidityTest = QList(prop,dynamicallyListeningSHAndValidityTest)
        }
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }
}

//the one for dynamic dependencies
//basically, the PE has a set of dynamic dependencies, and a varying dependency.
//this scheduling handler is the SH of: the varying PE, the determining element.
//all other dependencies are considered dynamic, and have one scheduling handler each. As such, the
class DynListeningSchedulingHandler(val p:PropagationElement, s:PropagationStructure) extends SimpleSchedulingHandler{
  s.registerSchedulingHandler(this)
  require(p.varyingDependencies, "expecting a PE with varying dependencies")

  var isDeterminingElementScheduled:Boolean = false


  override def scheduleSHForPropagation(sh:SimpleSchedulingHandler,isStillValid:()=>Boolean): Unit ={

  }



  //when this one is registered for run, it schedules the trigger for propagation
  //when it is enqueued for run, it enqueues the SHof the determining element
  //when the trigger is propagated, it calls this class to

  override def schedulePEForPropagation(pe:PropagationElement): Unit ={
    if(isRunning){
      //we are actually propagating
      runner.enqueuePE(pe)
    }else {
      if(pe == p.determiningElement) isDeterminingElementScheduled = true
      scheduledElements = QList(pe, scheduledElements)
      scheduleMyselfForPropagation()
    }
  }

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


class CalBackPropagationElement(callBackTarget:()=> Unit,
                                staticallyListeningElements:Iterable[PropagationElement],
                                staticallyListenedElements:Iterable[PropagationElement])
  extends PropagationElement(false){

  //We register this to be between the listened elements and the varying dependency PE
  //there is no dynamic dependency however because we do not want any notification
  // and because this PE will be scheduled by some external mechanism
  for(staticallyListened <- staticallyListenedElements){
    staticallyListened.registerStaticallyListeningElement(this)
  }
  for(staticallyListening <- staticallyListeningElements){
    this.registerStaticallyListeningElement(staticallyListening)
  }

  override protected def performPropagation(): Unit ={
    callBackTarget()
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

            val newSchedulingHandler = new SimpleSchedulingHandler()
            p.registerSchedulingHandler(newSchedulingHandler)
            pe.schedulingHandler = newSchedulingHandler

          } else {
            val referenceListeningSchedulingHandler = staticallyListeningElements.head.schedulingHandler
            staticallyListeningElements = staticallyListeningElements.tail
            while (staticallyListeningElements != null) {
              if (staticallyListeningElements.head.schedulingHandler != referenceListeningSchedulingHandler) {
                //there are more than one listening scheduling handler, so we create a scheduling handler on pe
                val newSchedulingHandler = new SimpleSchedulingHandler()
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
