package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dag.{ConcreteDAG, DAGNode}
import oscar.cbls.algo.quick.QList

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement],
                                 nbPE:Int,
                                 override val model:PropagationStructure)
  extends PropagationElement(PropagationImpactCharacteristics.SCCNotificationBehavior,false)
    with AbstractSchedulingHandler{

  val myRunner = new NaiveMonoThreadRunner(nbPE)
  val mySchedulingHandler = new SchedulingHandler()
  mySchedulingHandler.runner = myRunner

  model.registerPropagationElement(this)
  for (e <- propagationElements){
    e.scc = this
    e.schedulingHandler = mySchedulingHandler
  }

  // ////////////////////////////////////////////////////////////////////////
  // managing runnner, scheduling handler and propagation
  override def scheduleSHForPropagation(sh: SchedulingHandler): Unit ={
    require(sh == mySchedulingHandler)
    scheduleMyselfForPropagation()
  }

  override def performPropagation(): Unit ={
    injectAllWaitingDependencies()
    myRunner.runSH(mySchedulingHandler)
  }

  // ////////////////////////////////////////////////////////////////////////
  // managing the dynamic dependency graph and the incremental topological sort

  private val dAGStructure = new ConcreteDAG(propagationElements.asInstanceOf[QList[DAGNode]])
  dAGStructure.autoSort = true //we go on autoSort

  private var waitingDependenciesToInjectBeforePropagation: QList[WaitingDependency] = null

  class WaitingDependency(val from: PropagationElement,
                          val to: PropagationElement,
                          val isStillValid:() => Boolean,
                          val injector1:() => Unit,
                          var injector2:() => Unit)

  private var nextWaitingDependency:WaitingDependency = null

  private def injectWaitingDependencyIfStillValid(w:WaitingDependency): Unit ={
    if(w.isStillValid()){
      w.injector1()
      w.injector2()
      dAGStructure.notifyAddEdge(w.from, w.to)
    }
  }

  def registerOrCompleteWaitingDependency(from:PropagationElement,
                                          to:PropagationElement,
                                          injector:() => Unit,
                                          isStillValid:() => Boolean): Unit ={
    if (nextWaitingDependency == null) {
      nextWaitingDependency = new WaitingDependency(from,
        to,
        isStillValid,
        injector,
        null)
    }else{
      require(nextWaitingDependency.from == from)
      require(nextWaitingDependency.to == to)
      nextWaitingDependency.injector2 = injector

      if(from.layer >= to.layer){
        //the DAG does not obey this precedence, so we store it for later injection
        //since injecting dependencies might cause DAG to be cyclic, temporarily
        //as invariants generlly add and remove their dependencies in a non-ordered fashion
        waitingDependenciesToInjectBeforePropagation = QList(nextWaitingDependency(from, to),waitingDependenciesToInjectBeforePropagation)
      }else{
        //the DAG obeys the precedence, so we inject and notify to the DAG data structure
        injectWaitingDependencyIfStillValid(nextWaitingDependency)
      }
      nextWaitingDependency = null
    }
  }

  private def injectAllWaitingDependencies(){
    while(waitingDependenciesToInjectBeforePropagation!=null){
      injectWaitingDependencyIfStillValid(waitingDependenciesToInjectBeforePropagation.head)
      waitingDependenciesToInjectBeforePropagation = waitingDependenciesToInjectBeforePropagation.tail
    }
  }
}
