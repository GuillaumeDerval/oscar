package oscar.cbls.core.propagation.draft


import oscar.cbls.algo.dag.DAGNode
import oscar.cbls.algo.dll.{DPFDLLStorageElement, DelayedPermaFilteredDoublyLinkedList}
import oscar.cbls.algo.quick.QList


trait PseudoPropagationElement {
  def registerStaticallyListeningElement(listeningElement: PropagationElement)
  def registerDynamicallyListeningElement(listeningElement:PropagationElement,
                                          id:Int,
                                          determiningPermanentDependency:Boolean): KeyForDynamicDependencyRemoval
}

trait InactivePropagationElement extends PseudoPropagationElement{
  override def registerStaticallyListeningElement(listeningElement: PropagationElement): Unit = {}

  override def registerDynamicallyListeningElement(listeningElement: PropagationElement,
                                                   id: Int,
                                                   determiningPermanentDependency: Boolean): KeyForDynamicDependencyRemoval = ???
}


trait VaryingDependency extends PropagationElement{

}

abstract class PropagationElement(val varyingDependencies:Boolean) extends DAGNode {

  var uniqueID = -1 //DAG node already have this kind of stuff
  var isScheduled:Boolean = false
  var schedulingHandler:SimpleSchedulingHandler = null
  var model:PropagationStructure = null

  private[this] var myScc:StronglyConnectedComponent = null
  //We have a getter because a specific setter is define herebelow
  def scc:StronglyConnectedComponent = myScc

  var layer:Int = -1

  // //////////////////////////////////////////////////////////////////////
  //static propagation graph
  var staticallyListeningElements:QList[PropagationElement] = null
  var staticallyListenedElements:QList[PropagationElement] = null

  /**
    * listeningElement call this method to express that
    * they might register with dynamic dependency to the invocation target
    * @param listeningElement
    */
  def registerStaticallyListeningElement(listeningElement: PropagationElement) {
    staticallyListeningElements = QList(listeningElement,staticallyListeningElements)
    listeningElement.staticallyListenedElements = QList(this,listeningElement.staticallyListenedElements)
  }

  // //////////////////////////////////////////////////////////////////////
  //dynamic propagation graph

  val dynamicallyListenedElements: DelayedPermaFilteredDoublyLinkedList[PropagationElement]
  = new DelayedPermaFilteredDoublyLinkedList[PropagationElement]

  val dynamicallyListeningElements: DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]
  = new DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]

  /**
    * a PE that listens to another PE should call this method on the PE it listens
    */
  def registerTemporaryDynamicDependency(listeningElement:PropagationElement,
                                         id:Int): KeyForDynamicDependencyRemoval = {

    require(listeningElement.varyingDependencies, "cannot register with temporary dependency if no varying dependencies")
    new KeyForDynamicDependencyRemoval(
      this.dynamicallyListeningElements.addElem((listeningElement,id)), {
        val targetList = listeningElement.dynamicallyListenedElements
        if (targetList != null) targetList.addElem(this) else null
      })
  }

  def registerPermanentDynamicDependency(listeningElement:PropagationElement,
                                         id:Int){
    this.dynamicallyListeningElements.addElem((listeningElement,id))
    val targetList = listeningElement.dynamicallyListenedElements
    if (targetList != null) targetList.addElem(this)
  }

  // //////////////////////////////////////////////////////////////////////
  //DAG stuff, for SCC sort

  def compare(that: DAGNode): Int = {
    assert(this.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    assert(that.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    this.uniqueID - that.uniqueID
  }

  final var getDAGPrecedingNodes: Iterable[DAGNode] = null
  final var getDAGSucceedingNodes: Iterable[DAGNode] = null

  def scc_=(scc:StronglyConnectedComponent): Unit ={
    require(this.scc == null)
    this.scc = scc

    //we have to create the SCC injectors that will maintain the filtered Perma filter of nodes in the same SCC
    //for the listening side
    def filterForListening(listeningAndPayload: (PropagationElement, Int),
                           injector: (() => Unit),
                           isStillValid: (() => Boolean)) {
      val listening = listeningAndPayload._1
      if (scc == listening.scc) {
        scc.registerOrCompleteWaitingDependency(this, listening, injector, isStillValid)
      }
    }

    getDAGSucceedingNodes = dynamicallyListeningElements.delayedPermaFilter(filterForListening, (e) => e._1)

    getDAGPrecedingNodes = if(varyingDependencies) {
      def filterForListened(listened: PropagationElement,
                            injector: (() => Unit),
                            isStillValid: (() => Boolean)){
        if (scc == listened.scc) {
          scc.registerOrCompleteWaitingDependency(listened, this, injector, isStillValid)
        }
      }
      dynamicallyListenedElements.delayedPermaFilter(filterForListened)
    }else{
      staticallyListenedElements.filter(_.scc == scc)
    }
  }

  // ////////////////////////////////////////////////////////////////////////
  // to spare on memory (since we have somewhat memory consuming PE
  def dropUselessGraphAfterClose(): Unit ={
    staticallyListenedElements = null
    staticallyListeningElements = null
  }

  // ////////////////////////////////////////////////////////////////////////
  // api about scheduling and propagation

  def scheduleMyselfForPropagation(): Unit ={
    if(!isScheduled){
      isScheduled = true
      schedulingHandler.schedulePEForPropagation(this)
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

  final def propagate(){
    if(isScheduled) {
      isScheduled = false
      performPropagation()
    }
  }

  protected def performPropagation():Unit = ???
}


/**
  * This is the node type to be used for bulking
  * @author renaud.delandtsheer@cetic.be
  */
trait BulkPropagationElement extends PropagationElement {

}

/**
  * This class is used in as a handle to register and unregister dynamically to variables
  * @author renaud.delandtsheer@cetic.be
  */
class KeyForDynamicDependencyRemoval(key1: DPFDLLStorageElement[(PropagationElement, Int)],
                                     key2: DPFDLLStorageElement[PropagationElement]) {
  def performRemove(): Unit = {
    if(key1 != null) key1.delete()
    if(key2 != null) key2.delete()
  }
}
