package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

class PropagationStructure(val guaranteedAcyclic:Boolean) {

  var myIsClosed:Boolean = false

  def isClosed:Boolean = myIsClosed

  var allSchedulingHandlers: QList[SchedulingHandler] = null

  def registerSchedulingHandler(s: SchedulingHandler): Unit = {
    allSchedulingHandlers = QList(s, allSchedulingHandlers)
  }

  private[this] var nextUniqueIDForPropagationElement = 0
  def registerPropagationElement(pe:PropagationElement): Unit ={
    require(pe.uniqueID == -1)
    pe.uniqueID = nextUniqueIDForPropagationElement
    nextUniqueIDForPropagationElement += 1
    allPropagationElements = QList(pe,allPropagationElements)
  }
  var allPropagationElements: QList[PropagationElement] = null

  var layerToPropagationElements: Array[QList[PropagationElement]] = null
  var layerToNbClusteredPropagationElements: Array[Int] = null

  var stronglyConnectedComponents:QList[StronglyConnectedComponent] = null

  /**
    * Builds a dictionary to store data related to the PE.
    * the dictionary is O(1), based on an array.
    * It only works on PE that are registered to this structure.
    * The storage is not initialized, call the initialize to set it to some conventional value.
    * @tparam T the type stored in the data structure
    * @return a dictionary over the PE that are registered in the propagation structure.
    */
  def buildNodeStorage[T](implicit X: Manifest[T]): NodeDictionary[T]
  = new NodeDictionary[T](nextUniqueIDForPropagationElement)

  private var nbLayer: Int = -1
  private var globalRunner: Runner = null

  def close(): Unit = {
    require(!myIsClosed,"Propagation structure already closed")
    myIsClosed = true

    //1: identifier les SCC
    //cela rajoute des callBackPE et "supprime" des PE


    val (propagationElementsNotInSCC, stronglyConnectedComponents)
      = new SchedulingHandlerPartitioningAlgo(this).identifyAndInstantiateSCC()

    this.stronglyConnectedComponents = stronglyConnectedComponents

    //2: créer les variableDependencySH
    //cela rajoute des callBackPE
      new SchedulingHandlerPartitioningAlgo(this).instantiateVariableSchedulingHandlersForPENotInSCC()

    //3: faire le tri par couche
    (layerToNbClusteredPropagationElements,layerToPropagationElements)
      = new LayerSorterAlgo(
      propagationElementsNotInSCC).sortNodesByLayer()

    nbLayer = layerToPropagationElements.length
    require(layerToNbClusteredPropagationElements.length == nbLayer)

    //4: créer les autre SH en parcourant les couches
    new SchedulingHandlerPartitioningAlgo(this).partitionGraphIntoSchedulingHandlers()

    //finally, assign the globalRuner to all SH
    globalRunner = new LayerSortRunner(nbLayer)
    for (sh <- allSchedulingHandlers) {
      sh.globalRunner = globalRunner
    }
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
class NodeDictionary[T](val MaxNodeID: Int)(implicit val X: Manifest[T]) {
  private val storage: Array[T] = new Array[T](MaxNodeID + 1)

  def update(elem: PropagationElement, value: T) {
    storage(elem.uniqueID) = value
  }

  def get(elem: PropagationElement): T = storage(elem.uniqueID)

  def initialize(value: () => T) { for (i <- storage.indices) storage(i) = value() }
}

