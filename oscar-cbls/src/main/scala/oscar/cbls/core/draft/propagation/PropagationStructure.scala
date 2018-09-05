package oscar.cbls.core.draft.propagation

import oscar.cbls.algo.quick.QList


/**
  *
  * @param debug set to true, and all PE will be chacked after propagation. It also forces a total propagation every time a propagation is triggered, so everything will be slow
  * @param guaranteedAcyclic
  * @param dropStaticGraphAfterClose
  */
class PropagationStructure(val debug:Boolean,
                           val guaranteedAcyclic:Boolean,
                           val dropStaticGraphAfterClose:Boolean)
  extends LayerSortRunner() {

  private var myIsClosed:Boolean = false

  def isClosed:Boolean = myIsClosed

  // registering Scheduling handlers
  private var allSchedulingHandlers: QList[SchedulingHandler] = null

  def registerSchedulingHandler(s: SchedulingHandler): Unit = {
    allSchedulingHandlers = QList(s, allSchedulingHandlers)
  }

  //registering propagation elements
  private var myAllPropagationElements: QList[PropagationElement] = null

  // registering Propagation Elements
  private[this] var nextUniqueIDForPropagationElement = 0
  protected def registerPropagationElement(pe:PropagationElement): Unit ={
    require(pe.uniqueID == -1)
    pe.uniqueID = nextUniqueIDForPropagationElement
    nextUniqueIDForPropagationElement += 1
    myAllPropagationElements = QList(pe,myAllPropagationElements)
  }


  def allPropagationElements:QList[PropagationElement] = myAllPropagationElements

  // registering propagation layers
  private var stronglyConnectedComponents:QList[StronglyConnectedComponent] = null

  private var layerToPropagationElements:Array[QList[PropagationElement]] = null

  private var finalSchedulingHandlers:Iterable[SchedulingHandler] = null

  def close(): Unit = {
    require(!myIsClosed,"Propagation structure already closed")
    myIsClosed = true

    val myPartitioningAlgo = new SchedulingHandlerPartitioningAlgo(this)

    //1: identifier les SCC
    //cela rajoute des callBackPE et "supprime" des PE
    val (propagationElementsNotInSCC, stronglyConnectedComponents)
      = myPartitioningAlgo.identifyAndInstantiateSCC()

    this.stronglyConnectedComponents = stronglyConnectedComponents

    //2: créer les variableDependencySH
    //cela rajoute des callBackPE

    myPartitioningAlgo.instantiateVariableSchedulingHandlersForPENotInSCC()

    //3: faire le tri par couche
    val (layerToNbClusteredPropagationElements,tmpLayerToPropagationElements)
      = new LayerSorterAlgo(
      propagationElementsNotInSCC).sortNodesByLayer()

    layerToPropagationElements = tmpLayerToPropagationElements

    nbLayer = layerToPropagationElements.length

    require(layerToNbClusteredPropagationElements.length == nbLayer)

    //4: créer les autre SH en parcourant les couches
    //Et faire les registration des listening SH
    myPartitioningAlgo.partitionGraphIntoSchedulingHandlers(layerToPropagationElements)

    if(debug) {
      finalSchedulingHandlers = allSchedulingHandlers.filter(_.noListeningSchedulingHandler)
    }

    for(pe <- allPropagationElements){
      pe.reScheduleIfScheduled()
    }

    if(dropStaticGraphAfterClose){
      for(pe <- allPropagationElements){
        pe.dropUselessGraphAfterClose()
      }

    }
    assert(allPropagationElements.forall(_.schedulingHandler != null))
  }

  override def doRun(): Unit = {
    require(isClosed)
    if(debug) {
      for(sh <- finalSchedulingHandlers){
        sh.loadScheduledElementsAndAllSourcesIntoRunner()
      }
      super.doRun()
      for(pe <- allPropagationElements){
        pe.checkInternals()
      }
    }else {
      super.doRun()
    }
  }

  /**
    * Builds a dictionary to store data related to the PE.
    * the dictionary is O(1), based on an array.
    * It only works on PE that are registered to this structure.
    * The storage is not initialized, call the initialize to set it to some conventional value.
    *
    * @tparam T the type stored in the data structure
    * @param initValue the value stored for each propagation element
    * @return a dictionary over the PE that are registered in the propagation structure.
    */
  def buildFilledNodeStorage[@specialized(Boolean,Int) T:Manifest](initValue:T): NodeDictionary[T] = {
    new NodeDictionary[T](Array.fill[T](nextUniqueIDForPropagationElement)(initValue), allPropagationElements,this)
  }


  /**
    * Builds a dictionary to store data related to the PE.
    * the dictionary is O(1), based on an array.
    * It only works on PE that are registered to this structure.
    * The storage is not initialized, call the initialize to set it to some conventional value.
    *
    * @tparam T the type stored in the data structure
    * @param init a function to specify the value stored for each propagation element
    * @return a dictionary over the PE that are registered in the propagation structure.
    */
  def buildTabulatedNodeStorage[@specialized(Boolean,Int) T:Manifest](init:PropagationElement => T): NodeDictionary[T] = {
    val a = buildFilledNodeStorage[T](null.asInstanceOf[T]) //TODO: find a one pass way to do this (not so important)
    a.updateAll(init)
    a
  }
}

/**
  * This is a O(1) dictionary for propagation elements.
  * It is based on an array, and the keys it support is only the PE that have been registered
  * to the propagation structure by the time this is instantiated.
  * WARNING: this is not efficient if you do not actually use many of the keys
  * because the instantiated array will be very large compared to your benefits.
  * This might kill cache and RAM for nothing
  *
  * @param MaxNodeID the maximal ID of a node to be stored in the dictionary
  *                  (since it is O(1) it is an array, and we allocate the full necessary size
  * @tparam T the type stored in this structure
  * @author renaud.delandtsheer@cetic.be
  */
class NodeDictionary[@specialized(Boolean,Int) T:Manifest](storage: Array[T],
                                                           allConsideredPE: QList[PropagationElement],
                                                           val ps:PropagationStructure) {

  def apply(elem: PropagationElement): T = storage(elem.uniqueID)

  def update(elem: PropagationElement, value: T) : Unit ={
    storage(elem.uniqueID) = value
  }

  def updateAll(value: PropagationElement => T) : Unit ={
    var a = allConsideredPE
    while(a != null){
      val elem = a.head
      storage(elem.uniqueID) = value(elem)
      a = a.tail
    }
  }

  def setAll(value:T): Unit ={
    var i = storage.length
    while(i >0){
      i = i-1
      storage(i) = value
    }
  }
}
