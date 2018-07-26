package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

abstract class SchedulingHandler(val isSCC:Boolean){

  var globalRunner:Runner = null

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
      globalRunner.doRun()
      notifyEndRun()
    }
  }

  def loadScheduledElementsAndAllSourcesIntoRunner()
  def notifyEndRun()

  def scheduledAndNotRunning:Boolean = isScheduled && ! isRunning

  protected[this] var isScheduled:Boolean = false
  protected[this] var isRunning:Boolean = false


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


class SimpleSchedulingHandler() extends SchedulingHandler(isSCC=false){

  override def loadScheduledElementsAndAllSourcesIntoRunner(){
    if (isScheduled && !isRunning) {
      //could be not scheduled anymore since SH might be listened by several SH
      //could already be running, for the same reason.

      isRunning = true
      globalRunner.enqueue(scheduledElements)
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
      globalRunner.enqueuePE(pe)
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
class SchedulingHandlerForPEWithVaryingDependencies(val p:PropagationElement with VaryingDependencies)
  extends SchedulingHandler(isSCC = false){


  val callBackPE = new CalBackPropagationElement(notificationThatAllRunLoadedDependenciesAreUpToDate,

  )

  // /////////////////////////////////////////////////////////
  // running


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
        globalRunner.enqueuePE(callBackPE)
        return
      }
    }

    if(loadScheduledDynamicallyListenedElementsIntoRunner()){
      globalRunner.enqueuePE(callBackPE)
      return
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
  var pERequiresPropagation = p.isScheduled

  def schedulePEForPropagation(pe:PropagationElement): Unit ={
    require(pe == p)
    if(isRunning){
      //we are actually propagating
      globalRunner.enqueuePE(pe)
    }else {
      pERequiresPropagation = true
      scheduleMyselfForPropagation()
    }
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

