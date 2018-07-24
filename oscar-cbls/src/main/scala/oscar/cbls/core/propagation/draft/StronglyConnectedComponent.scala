package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dag.{ConcreteDAG, DAGNode}
import oscar.cbls.algo.quick.QList

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement],
                                 nbPE:Int,
                                 override val model:PropagationStructure)
  extends PropagationElement()
    with SchedulingHandler{


  //what is the scheduling handler of an SCC?
  //I propose that a SCC has no scheduling handler at all since it is a scheduling handler it itself


  private[this] val runnerForMyPropagationElements = new TotalOrderRunner(nbPE)
  private[this] val schedulingHandlerForMyPropagationElements = new SimpleSchedulingHandler()
  schedulingHandlerForMyPropagationElements.runner = runnerForMyPropagationElements

  model.registerPropagationElement(pe = this)

  for (e <- propagationElements){
    e.scc = this //SCC is also set as the scheduling handler through this method
  }

  // ////////////////////////////////////////////////////////////////////////
  // behaving like a scheduling handler from the viewpoint of propagation elements
  // by forwarding all API calls to "mySchedulingHandler"

  override def schedulePEForPropagation(pe: PropagationElement): Unit = {
    schedulingHandlerForMyPropagationElements.schedulePEForPropagation(pe)
  }

  override def isSCC: Boolean = true



  //Called when a source sh has some updates.
  override def scheduleSHForPropagation(sh: SchedulingHandler, isStillValid: () => Boolean): Unit = {
    schedulingHandlerForMyPropagationElements.scheduleSHForPropagation(sh,isStillValid)
  }



  override def loadScheduledElementsAndAllSourcesIntoRunner(): Unit = {

  }

  // ////////////////////////////////////////////////////////////////////////
  // the part about propagation element that perform propagation

  override def performPropagation(): Unit ={
    injectAllWaitingDependencies()
    mySchedulingHandler.
    myRunner.runSH(mySchedulingHandler)
  }

  // ////////////////////////////////////////////////////////////////////////
  // managing the dynamic dependency graph and the incremental topological sort

  private val dAGStructure = new ConcreteDAG(propagationElements.asInstanceOf[QList[DAGNode]])
  dAGStructure.autoSort = true //we activate the autoSort, of course

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

    scheduleMyselfForPropagation()

    //we know that there will be two calls to this one: one for the listening one and one for the listened one.

    if (nextWaitingDependency == null) {
      nextWaitingDependency = new WaitingDependency(from,
        to,
        isStillValid,
        injector1 = injector,
        injector2 = null)
    }else{
      require(nextWaitingDependency.from == from)
      require(nextWaitingDependency.to == to)
      nextWaitingDependency.injector2 = injector

      if(from.positionInTopologicalSort >= to.positionInTopologicalSort){
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
    require(nextWaitingDependency == null)
    while(waitingDependenciesToInjectBeforePropagation!=null){
      injectWaitingDependencyIfStillValid(waitingDependenciesToInjectBeforePropagation.head)
      waitingDependenciesToInjectBeforePropagation = waitingDependenciesToInjectBeforePropagation.tail
    }
  }
}
