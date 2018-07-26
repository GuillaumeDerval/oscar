package oscar.cbls.core.propagation.draft


import oscar.cbls.algo.dag.DAGNode
import oscar.cbls.algo.dll.{DPFDLLStorageElement, DelayedPermaFilteredDoublyLinkedList}
import oscar.cbls.algo.quick.QList

trait PseudoPropagationElement {
  def registerStaticallyListeningElement(listeningElement: PropagationElement){}
  def registerDynamicallyListeningElement(listeningElement:PropagationElement,
                                          id:Int,
                                          determiningPermanentDependency:Boolean): DPFDLLStorageElement[_] = null
}

abstract class PropagationElement() extends DAGNode with PseudoPropagationElement{

  override var uniqueID = -1 //DAG node already have this kind of stuff
  var isScheduled:Boolean = false

  private var mySchedulingHandler:SchedulingHandler = null

  def schedulingHandler_=(sh:SchedulingHandler): Unit ={
    require(mySchedulingHandler == null || !mySchedulingHandler.isSCC)
    mySchedulingHandler = sh
  }

  def schedulingHandler:SchedulingHandler = mySchedulingHandler

  var model:PropagationStructure = null

  //this is the position used for propagation, used both within SCC and out of SCC
  //it is read by all runners
  var propagationPosition:Int = -1

  // //////////////////////////////////////////////////////////////////////
  //static propagation graph
  var staticallyListeningElements:QList[PropagationElement] = null
  var staticallyListenedElements:QList[PropagationElement] = null

  def addStaticallyListenedElement(listened:PropagationElement): Unit ={
    staticallyListenedElements = QList(listened,staticallyListenedElements)
  }

  /**
    * listeningElement call this method to express that
    * they might register with dynamic dependency to the invocation target
    * @param listeningElement
    */
  override def registerStaticallyListeningElement(listeningElement: PropagationElement) {
    staticallyListeningElements = QList(listeningElement,staticallyListeningElements)
    listeningElement.addStaticallyListenedElement(this)
  }

  // //////////////////////////////////////////////////////////////////////
  //dynamic propagation graph

  private[this] val dynamicallyListeningElements: DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]
  = new DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]

  protected[propagation] def registerTemporaryDynamicDependencyListenedSide(listeningElement:VaryingDependencies,
                                                                            id:Int):  DPFDLLStorageElement[_] = {
    this.dynamicallyListeningElements.addElem((listeningElement,id))
  }

  def registerNotificationTargetForNewListeningPE(notificationMethod:((PropagationElement,Int),()=> Boolean) => Unit): Unit ={
    dynamicallyListeningElements.notifyInserts(notificationMethod)
  }

  protected[propagation] def registerPermanentDynamicDependencyListenedSide(listeningElement:PropagationElement ,
                                                                            id:Int) {
    this.dynamicallyListeningElements.addElem((listeningElement,id))
  }

  protected[propagation] def registerPermanentDynamicDependencyListeningSide(listenedElement:PropagationElement): Unit ={
    //nothing to do here, we always listen to the same elements,
    // so our dependencies are captures by statically listened elements
    //dynamic dependencies PE will need to do womthing here, actually and that's why there is this method
    //this call is not going to be performed many times because it is about permanent dependency,
    // so only called at startup and never during search
  }

  protected def registerPermanentDynamicDependency(listeningElement:PropagationElement,id:Int): Unit ={
    registerPermanentDynamicDependencyListenedSide(listeningElement,id)
    listeningElement.registerPermanentDynamicDependencyListeningSide(this)
  }

  protected def registerTemporaryDynamicDependency(listeningElement:VaryingDependencies, id:Int): KeyForDynamicDependencyRemoval ={
    new KeyForDynamicDependencyRemoval(
      registerTemporaryDynamicDependencyListenedSide(listeningElement:VaryingDependencies,id:Int),
      listeningElement.registerTemporaryDynamicDependencyListeningSide(this))
  }

  // //////////////////////////////////////////////////////////////////////
  //DAG stuff, for SCC sort

  //We have a getter because a specific setter is define herebelow
  def scc:Option[StronglyConnectedComponent] = schedulingHandler match {
    case scc: StronglyConnectedComponent => Some(scc)
    case _ => None
  }

  def scc_=(scc:StronglyConnectedComponent): Unit = {
    require(this.schedulingHandler == null)
    schedulingHandler = scc
    initiateDAGSucceedingNodesAfterSccDefinition(scc)
    initiateDAGPrecedingNodesAfterSCCDefinition(scc)
  }

  override def positionInTopologicalSort: Int = propagationPosition
  override def positionInTopologicalSort_=(newValue: Int): Unit = {propagationPosition = newValue}

  def compare(that: DAGNode): Int = {
    assert(this.model == that.asInstanceOf[PropagationElement].model)
    assert(this.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    assert(that.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    this.uniqueID - that.uniqueID
  }

  final var getDAGPrecedingNodes: Iterable[DAGNode] = null
  final var getDAGSucceedingNodes: Iterable[DAGNode] = null

  private def initiateDAGSucceedingNodesAfterSccDefinition(scc:StronglyConnectedComponent) {
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
  }

  private def initiateDAGPrecedingNodesAfterSCCDefinition(scc:StronglyConnectedComponent){
    getDAGPrecedingNodes = staticallyListenedElements.filter(_.scc == scc)
  }

  // ////////////////////////////////////////////////////////////////////////
  // to spare on memory (since we have somewhat memory consuming PE
  def dropUselessGraphAfterClose(): Unit ={
    staticallyListenedElements = null
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

  def ensureUpToDate(){
    schedulingHandler.ensureUpToDateStartPropagationIfNeeded()
  }

  final def propagate(){
    require(isScheduled)
    isScheduled = false
    //perform propagation might reschedule the stuff, so we updateisScheduled before calling performPropagation
    performPropagation()
  }

  protected def performPropagation():Unit = ???
}


trait VaryingDependencies extends PropagationElement{

  var permanentListenedPE:QList[PropagationElement] = null

  val dynamicallyListenedElements: DelayedPermaFilteredDoublyLinkedList[PropagationElement]
  = new DelayedPermaFilteredDoublyLinkedList[PropagationElement]

  override protected[propagation] def registerPermanentDynamicDependencyListeningSide(listenedElement:PropagationElement){
    dynamicallyListenedElements.addElem(listenedElement)
    permanentListenedPE = QList(listenedElement,permanentListenedPE)
  }

  protected[propagation] def registerTemporaryDynamicDependencyListeningSide(listenedElement:PropagationElement):DPFDLLStorageElement[_] = {
    dynamicallyListenedElements.addElem(listenedElement)
  }

  private def initiateDAGPrecedingNodesAfterSCCDefinition(scc:StronglyConnectedComponent){

    def filterForListened(listened: PropagationElement,
                          injector: (() => Unit),
                          isStillValid: (() => Boolean)){
      listened.scc match{
        case Some(otherScc) if scc == otherScc =>
          scc.registerOrCompleteWaitingDependency(listened, this, injector, isStillValid)
        case _ => ;
      }
    }
    getDAGPrecedingNodes = dynamicallyListenedElements.delayedPermaFilter(filterForListened)
  }

  override protected[propagation] def dropUselessGraphAfterClose(): Unit ={
    staticallyListenedElements = null
    staticallyListeningElements = null
  }
}


/**
  * This is the node type to be used for bulking
  * @author renaud.delandtsheer@cetic.be
  */
trait BulkPropagationElement extends PropagationElement {

}


class CalBackPropagationElement(callBackTarget:()=> Unit,
                                staticallyListeningElements:Iterable[PropagationElement],
                                staticallyListenedElements:Iterable[PropagationElement])
  extends PropagationElement(){

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

/**
  * This class is used in as a handle to register and unregister dynamically to variables
  * @author renaud.delandtsheer@cetic.be
  */
class KeyForDynamicDependencyRemoval(key1: DPFDLLStorageElement[_],
                                     key2: DPFDLLStorageElement[_]) {
  def performRemove(): Unit = {
    key1.delete()
    key2.delete()
  }
}
