package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dag.{ConcreteDAG, DAGNode}
import oscar.cbls.algo.quick.QList

abstract sealed class SchedulingHandler(val isSCC:Boolean, structure:PropagationStructure){
  structure.registerSchedulingHandler(this)

  /**this is teh call bach.
    * this did tell sh that this listens to sh, so now, sh is telling us that it has some updates
    */
  def scheduleListenedSHForPropagation(listenedSH:SchedulingHandler)

  /**
    * called by PE that have this as sh
    */
  def schedulePEForPropagation(pe:PropagationElement): Unit

  def ensureUpToDateStartPropagationIfNeeded(): Unit = {
    if(scheduledAndNotRunning){
      //it is scheduled, so there is something changed either in this SH or in listened SH; so we trigger propagation
      loadScheduledElementsAndAllSourcesIntoRunner()
      structure.doRun()
      notifyEndRun()
    }
  }

  def loadScheduledElementsAndAllSourcesIntoRunner()
  def notifyEndRun()

  @inline
  def scheduledAndNotRunning:Boolean = isScheduled && ! isRunning

  var isScheduled:Boolean = false
  var isRunning:Boolean = false

  private[this] var listeningSchedulingHandlers: QList[SchedulingHandler] = null

  /**a SH that is close ro obj registers here to this,
    * so it can be told that this SH
    * (or something that this SH listens to) has some updates
    * */
  def addListeningSchedulingHandler(sh: SchedulingHandler) {
    listeningSchedulingHandlers = QList(sh, listeningSchedulingHandlers)
  }

  protected def scheduleMyselfForPropagation(): Unit = {
    if (!isScheduled) {
      isScheduled = true
      var listeningSchedulingHandlersAcc = listeningSchedulingHandlers
      while (listeningSchedulingHandlersAcc != null) {
        listeningSchedulingHandlersAcc.head.scheduleListenedSHForPropagation(this)
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }
}


class SimpleSchedulingHandler(model:PropagationStructure)
  extends SchedulingHandler(isSCC=false,model){

  override def loadScheduledElementsAndAllSourcesIntoRunner(){
    if (isScheduled && !isRunning) {
      //could be not scheduled anymore since SH might be listened by several SH
      //could already be running, for the same reason.

      isRunning = true

      model.enqueue(scheduledElements)
      scheduledElements = null

      var toScheduleSHChildren = scheduledSHChildren
      //We do not remove scheduled SHChildren
      //because they will be explored in the notifyEndRun method as well
      while (toScheduleSHChildren != null) {
        val sh = toScheduleSHChildren.head
        toScheduleSHChildren = toScheduleSHChildren.tail
        sh.loadScheduledElementsAndAllSourcesIntoRunner()
        scheduledSHChildren = QList(sh,scheduledSHChildren)
      }
    }
  }

  override def notifyEndRun(){
    //it could be the case that it is scheduled and not running
    // if the dependency was not valid anymore when the run was done
    if(isRunning) {
      require(isScheduled)
      require(scheduledElements == null)
      isScheduled = false
      isRunning = false

      while (scheduledSHChildren != null) {
        scheduledSHChildren.head.notifyEndRun()
        scheduledSHChildren = scheduledSHChildren.tail
      }
    }
  }

  // /////////////////////////////////////////////////////////
  //PE scheduling

  protected [this] var scheduledElements:QList[PropagationElement] = null

  def schedulePEForPropagation(pe:PropagationElement): Unit ={
    if(isRunning){
      //we are actually propagating
      model.enqueuePE(pe)
    }else {
      scheduledElements = QList(pe, scheduledElements)
      scheduleMyselfForPropagation()
    }
  }

  // /////////////////////////////////////////////////////////
  // scheduling listened SH that notify about some change

  private[this] var scheduledSHChildren:QList[SchedulingHandler] = null

  def scheduleListenedSHForPropagation(listenedSH:SchedulingHandler){
    scheduledSHChildren = QList(listenedSH,scheduledSHChildren)

    if (isRunning) {
      //We are actually running, so forward ASAP to the globalRunner
      listenedSH.loadScheduledElementsAndAllSourcesIntoRunner()
    } else {
      scheduleMyselfForPropagation()
    }
  }
}



//the one for dynamic dependencies
//basically, the PE has a set of dynamic dependencies, and a varying dependency.
//this scheduling handler is the SH of: the varying PE, the determining element.
//all other dependencies are considered dynamic, and have one scheduling handler each. As such, the
class SchedulingHandlerForPEWithVaryingDependencies(val p:PropagationElement with VaryingDependencies,
                                                    structure:PropagationStructure)
  extends SchedulingHandler(isSCC = false,structure){

  val myCallBackPE = new CalBackPropagationElement(notificationThatAllRunLoadedDependenciesAreUpToDate,
    staticallyListeningElements = List(p),
    staticallyListenedElements = p.staticallyListenedElements
  )

  structure.registerPropagationElement(myCallBackPE)

  //les determiningElement sont renommés en permanentListenedPE
  override def loadScheduledElementsAndAllSourcesIntoRunner() {
    if (isScheduled && !isRunning) {
      //someone tells us to do the run, so we start it
      //if someone tells us to start the run and we are already ongoing, we do nothing
      isRunning = true
      loadScheduledDependenciesIntoRunner(loadDeterminingFirst = true)
    }
  }

  def notificationThatAllRunLoadedDependenciesAreUpToDate(): Unit = {
    loadScheduledDependenciesIntoRunner(loadDeterminingFirst=false)
  }

  private def loadScheduledDependenciesIntoRunner(loadDeterminingFirst:Boolean): Unit ={
    require(isRunning && isScheduled)

    if(loadDeterminingFirst){
      //we specifically load all scheduled and not ran SCC of determining elements
      if(loadScheduledDeterminingElementsIntoRunner()){
        structure.enqueuePE(myCallBackPE)
        return
      }
    }

    if(loadScheduledDynamicallyListenedElementsIntoRunner()){
      structure.enqueuePE(myCallBackPE)
      return
    }

    if(p.isScheduled){
      structure.enqueuePE(p)
    }
  }

  def loadScheduledDeterminingElementsIntoRunner():Boolean = {

    var detPEs = p.permanentListenedPE
    var anythingLoaded:Boolean = false

    while(detPEs!=null){
      anythingLoaded = anythingLoaded || loadSHOfPropagationElementIfNeeded(detPEs.head)
      detPEs = detPEs.tail
    }
    anythingLoaded
  }

  def loadScheduledDynamicallyListenedElementsIntoRunner():Boolean = {
    var anythingLoaded:Boolean = false

    val headPhantom = p.dynamicallyListenedElements.headPhantom
    var currentElement = headPhantom.next

    while(currentElement != headPhantom){
      anythingLoaded = anythingLoaded || loadSHOfPropagationElementIfNeeded(currentElement.elem)
      currentElement = currentElement.next
    }
    anythingLoaded
  }

  private var listenedSHsLoadedIntoRunner:QList[SchedulingHandler] = null

  /**
    * loads the SH of the pe if the SH is scheduled and not running
    * if something is loaded into the runner, returns true, otherwise returns false
    * if something xas loaded, the SH is stored into runLoadedSH
    * @param pe
    * @return
    */
  def loadSHOfPropagationElementIfNeeded(pe:PropagationElement):Boolean = {

    val sh = pe.schedulingHandler
    if(sh.scheduledAndNotRunning){
      sh.loadScheduledElementsAndAllSourcesIntoRunner()
      listenedSHsLoadedIntoRunner = QList(sh,listenedSHsLoadedIntoRunner)
      true
    }else false
  }

  override def notifyEndRun(){
    //it could be the case that it is scheduled and not running
    // if the dependency was not valid anymore when the run was done

    if(isRunning) {
      require(isScheduled)
      isScheduled = false
      isRunning = false

      while (listenedSHsLoadedIntoRunner != null) {
        listenedSHsLoadedIntoRunner.head.notifyEndRun()
        listenedSHsLoadedIntoRunner = listenedSHsLoadedIntoRunner.tail
      }
    }
  }


  // /////////////////////////////////////////////////////////
  //PE scheduling

  def schedulePEForPropagation(pe:PropagationElement): Unit ={
    require(pe == p)
    if (!isRunning) {
      scheduleMyselfForPropagation()
    }
    //we do not save this info since proper scheduling
    // will be performed during the run procedure
  }

  // /////////////////////////////////////////////////////////
  // scheduling listened SH that notify about some change

  def scheduleListenedSHForPropagation(listenedSH:SchedulingHandler){
    //we do not store anything, the things will be identified at propagation time,
    // by exploring teh dependencies of p
    //we just keep in mind that we should schedule this for propagation as well
    if (!isRunning) {
      scheduleMyselfForPropagation()
    }
  }
}

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement],
                                 nbPE:Int,
                                 structure:PropagationStructure)
  extends SchedulingHandler(isSCC=true,structure){

  //what is the scheduling handler of an SCC?
  //I propose that a SCC has no scheduling handler at all since it is a scheduling handler it itself

  val myCallBackPE = {
    var allStaticallyListenedElements:QList[PropagationElement] = null
    var allStaticallyListeningElements:QList[PropagationElement] = null

    for(pe <- propagationElements){
      for(listened <- pe.staticallyListenedElements){
        allStaticallyListenedElements = QList(listened,allStaticallyListenedElements)
      }

      for(listening <- pe.staticallyListeningElements){
        allStaticallyListeningElements = QList(listening,allStaticallyListeningElements)
      }
    }

    new CalBackPropagationElement(performSCCPropagation,
      staticallyListeningElements = allStaticallyListeningElements,
      staticallyListenedElements = allStaticallyListenedElements
    )
  }

  structure.registerPropagationElement(myCallBackPE)

  private[this] val runnerForMyPropagationElements = new TotalOrderRunner(nbPE)

  for (e <- propagationElements){
    e.scc = this //SCC is also set as the scheduling handler through this method
  }

  // ////////////////////////////////////////////////////////////////////////

  protected [this] var scheduledElements:QList[PropagationElement] = null

  override def schedulePEForPropagation(pe: PropagationElement): Unit = {
    if(sCCPropagationRunning){
      //we are actually propagating
      structure.enqueuePE(pe)
    }else {
      scheduledElements = QList(pe, scheduledElements)
      scheduleMyselfForPropagation()
    }
  }

  override def loadScheduledElementsAndAllSourcesIntoRunner(): Unit = {
    //we only load the callBack
    structure.enqueuePE(myCallBackPE)

    var toScheduleSHChildren = scheduledSHChildren
    //We do not remove scheduled SHChildren
    //because they will be explored in the notifyEndRun method as well
    while (toScheduleSHChildren != null) {
      val sh = toScheduleSHChildren.head
      toScheduleSHChildren = toScheduleSHChildren.tail
      sh.loadScheduledElementsAndAllSourcesIntoRunner()
      scheduledSHChildren = QList(sh,scheduledSHChildren)
    }
  }

  // ////////////////////////////////////////////////////////////////////////
  // the part about propagation element that perform propagation

  var sCCPropagationRunning:Boolean = false
  def performSCCPropagation(): Unit = {
    sCCPropagationRunning = true
    injectAllWaitingDependencies()

    runnerForMyPropagationElements.enqueue(scheduledElements)
    scheduledElements = null

    runnerForMyPropagationElements.doRun()
    sCCPropagationRunning = false
  }

  override def notifyEndRun() {
    //it could be the case that it is scheduled and not running
    // if the dependency was not valid anymore when the run was done
    if (isRunning) {
      require(isScheduled)
      require(scheduledElements == null)
      isScheduled = false
      isRunning = false

      while (scheduledSHChildren != null) {
        scheduledSHChildren.head.notifyEndRun()
        scheduledSHChildren = scheduledSHChildren.tail
      }
    }
  }

  // /////////////////////////////////////////////////////////
  // scheduling listened SH that notify about some change

  private[this] var scheduledSHChildren:QList[SchedulingHandler] = null

  def scheduleListenedSHForPropagation(listenedSH:SchedulingHandler){
    scheduledSHChildren = QList(listenedSH,scheduledSHChildren)

    if (isRunning) {
      //We are actually running, so forward ASAP to the globalRunner
      listenedSH.loadScheduledElementsAndAllSourcesIntoRunner()
    } else {
      scheduleMyselfForPropagation()
    }
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
