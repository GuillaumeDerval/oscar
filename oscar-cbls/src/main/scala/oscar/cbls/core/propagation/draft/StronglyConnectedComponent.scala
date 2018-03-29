package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dag.{ConcreteDAG, DAG, DAGNode}
import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement],
                                 nbPE:Int,
                                 override val model:PropagationStructure)
  extends PropagationElement(PropagationImpactCharacteristics.SCCNotificationBehavior){

  val myCustomRunner = new NaiveMonoThreadRunner(nbPE,this)
  val myCustomSchedulingHandler = new RegularSchedulingHandler()

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





