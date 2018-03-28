package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dag.{ConcreteDAG, DAG, DAGNode}
import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement],
                                 nbPE:Int,
                                 override val model:PropagationStructure)
  extends PropagationElement(PropagationImpactCharacteristics.SCCNotificationBehavior){

  val myCustomRunner = new NaiveMonoThreadRunner(nbPE,this)

  model.registerPropagationElement(this)
  for (e <- propagationElements) e.scc = this

  val dAGStructure = new ConcreteDAG(propagationElements.asInstanceOf[QList[DAGNode]])

  var waitingDependenciesToInjectBeforePropagation: QList[WaitingDependency] = null

  case class WaitingDependency(from: PropagationElement,to: PropagationElement)

  //call this just after any dependency is added between PE of the SCC
  def dependencyAdded(from: PropagationElement, to: PropagationElement) {
    require(from.scc == this)
    require(to.scc == this)
    require(from != to)

    if(from.layer >= to.layer){
      //the DAG does not obey thid precedence, so we store it for later injection
      waitingDependenciesToInjectBeforePropagation = QList(WaitingDependency(from, to),waitingDependenciesToInjectBeforePropagation)
    }
  }

  def injectAllWaitingDependencies(){
    while(waitingDependenciesToInjectBeforePropagation!=null){
      val waiting = waitingDependenciesToInjectBeforePropagation.head
      waitingDependenciesToInjectBeforePropagation = waitingDependenciesToInjectBeforePropagation.tail
      if (waiting.from.layer >= waiting.to.layer) {
        //does not obey thid dependency, so we inject it.
        dAGStructure.notifyAddEdge(waiting.from, waiting.to)
      }
    }
  }

  override def propagate(): Unit ={
    injectAllWaitingDependencies()
    myCustomRunner.run(null)
  }
}





class StronglyConnectedComponent(val propagationElements: Iterable[PropagationElement],
                                 val core: PropagationStructure, val _UniqueID: Int)
  extends PropagationElement with SchedulingHandler with DAG {

  schedulingHandler = core
  uniqueID = _UniqueID

  for (e <- propagationElements) e.schedulingHandler = this

  def size: Int = propagationElements.size

  override def propagationStructure: PropagationStructure = core

  var scheduledElements: QList[PropagationElement] = null

  for (e <- propagationElements) {
    e.setInSortingSCC()
  }
  for (e <- propagationElements) {
    e.initiateDynamicGraphFromSameComponent(this)
  }

  //for the DAG
  override def nodes = propagationElements.asInstanceOf[Iterable[DAGNode]]

  var newDependenciesToInject: List[WaitingDependency] = List.empty

  case class WaitingDependency(from: PropagationElement,
                               to: PropagationElement,
                               var inject1: (() => Unit) = null,
                               var inject2: (() => Unit) = null,
                               var isStillValid: (() => Boolean) = null) {
    /**
      * injects the waiting dependency
      * @return true if the dependency was injected, false otherwise
      */
    def injectIfStillValid(): Boolean = {
      if (isStillValid()) {
        inject1()
        inject2()
        true
      } else false
    }

    def inject() {
      inject1()
      inject2()
    }
  }

  def injectWaitingNewDependencies(autoSort: Boolean) {
    for (d: WaitingDependency <- newDependenciesToInject) {
      if (d.injectIfStillValid() && autoSort) notifyAddEdge(d.from, d.to)
    }
    newDependenciesToInject = List.empty
  }

  def registerListenedWaitingDependency(injector: (() => Unit), isStillValid: (() => Boolean)) {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      waiting.inject1 = injector
      waiting.isStillValid = isStillValid
    } else {
      injector()
    }
  }

  def registerListeningWaitingDependency(injector: (() => Unit)) {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      waiting.inject2 = injector
    } else {
      injector()
    }
  }

  def addDependency(from: PropagationElement, to: PropagationElement) {
    if (autoSort) {
      newDependenciesToInject = WaitingDependency(from, to) :: newDependenciesToInject
    }
  }

  /**
    * this is called when the dependency has been added and all its field are filled.
    * We take the opportunity to check if the dependency is by any chance already implemented
    * in the sort.
    * if yes, we inject it right away, since it does not trigger any computation, actually.
    */
  def dependencyAdded() {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      if (waiting.from.position < waiting.to.position) {
        waiting.inject()
        notifyAddEdge(waiting.from, waiting.to)
        newDependenciesToInject = newDependenciesToInject.tail
      }
    }
  }

  val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.position, size)

  override def performPropagation() {
    //setting autosort to true will not perform any operation unless it was set to false. This happens in two cases:
    //at the initial propagation, and when a stall just occurred. In these case, a non-incremental sort takes place

    injectWaitingNewDependencies(autoSort)
    autoSort = true

    var currentPos = scheduledElements
    while (currentPos != null) {
      h.insert(currentPos.head)
      currentPos = currentPos.tail
    }
    scheduledElements = null

    var maxposition: Int = -1

    while (!h.isEmpty) {
      val x = h.popFirst()
      x.propagate()
      assert(x.position >= maxposition, "non monotonic propagation detected in SCC")
      assert({ maxposition = x.position; true })

      var currentPos = scheduledElements
      while (currentPos != null) {
        h.insert(currentPos.head)
        currentPos = currentPos.tail
      }
      scheduledElements = null
    }
  }


  def scheduleForPropagation(element: PropagationElement) {
    scheduledElements = QList(element, scheduledElements)
    super.scheduleForPropagation()
  }

  override def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (element <- propagationElements) {
      toreturn = element.decrementSucceedingAndAccumulateFront(toreturn)
    }
    toreturn
  }

  override def setCounterToPrecedingCount(): Boolean = {
    position = propagationElements.count(p => p.setCounterToPrecedingCount())
    position != 0
  }

  override private[core] def rescheduleIfScheduled() {}
  //we do nothing, since it is the propagation elements that trigger the registration if needed of SCC

  override def checkInternals(){
    for (e <- propagationElements) { e.checkInternals() }
  }

  def stats:String = {
    "{" + "\n    " + propagationElements.map(_.getClass.getSimpleName).groupBy((name: String) => name).map(a => a._1 + ":" + a._2.size).mkString("\n    ") + "\n  }"
  }
}