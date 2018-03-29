package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

class PropagationStructure(nbSystemThread:Int,noCycle:Boolean) extends SchedulingHandler {

  var allSchedulingHandlersNotSCC: QList[SchedulingHandler] = null

  def registerSchedulingHandler(s: SchedulingHandler): Unit = {
    allSchedulingHandlersNotSCC = QList(s, allSchedulingHandlersNotSCC)
  }

  private[this] var nextUniqueID = 0
  def registerPropagationElement(pe:PropagationElement): Unit ={
    require(pe.uniqueID == -1)
    pe.uniqueID = nextUniqueID
    nextUniqueID += 1
    allPropagationElements = QList(pe,allPropagationElements)
  }
  var allPropagationElements: QList[PropagationElement]
  var clusteredPropagationElements:QList[PropagationElement]
  var nbCLusteredPEs:Int

  var layerToPropagationElements: Array[QList[PropagationElement]] = null
  var layerToNbPropagationElements: Array[Int] = null



  /**
    * Builds a dictionary to store data related to the PE.
    * the dictionary is O(1), based on an array.
    * It only works on PE that are registered to this structure.
    * The storage is not initialized, call the initialize to set it to some conventional value.
    * @tparam T the type stored in the data structure
    * @return a dictionary over the PE that are registered in the propagation structure.
    */
  def buildNodeStorage[T](implicit X: Manifest[T]): NodeDictionary[T] = new NodeDictionary[T](nextUniqueID)



  //can only be called when all SH are created
  override def runner_=(runner: Runner) {
    super.runner_=(runner)
    for (s <- allSchedulingHandlersNotSCC) {
      s.runner = runner
    }
  }


  def registerForPartialPropagation(pe: PropagationElement): Unit = {
    //we root a schedulingHandler at this node.
  }

  private var nbLayer: Int = -1
  private var runner: Runner = null


  def close(): Unit = {
    (clusteredPropagationElements,nbCLusteredPEs) = new SCCIdentifierAlgo(allPropagationElements,this).identifySCC()

    instantiateVSH()

    (layerToNbPropagationElements,layerToPropagationElements)= new LayerSorterAlgo(clusteredPropagationElements,nbCLusteredPEs,noCycle).sortNodesByLayer()


    partitionGraphIntoSchedulingHandlers()

    //create runner and multiTreaded partition (if multi-treading)
    runner = if (nbSystemThread == 1) {
      new MonoThreadRunner(nbLayer)
    } else {
      new MultiThreadRunner(nbSystemThread, new MultiTreadingPartitioningAlgo(layerToPropagationElements, layerToNbPropagationElements).partitionGraphIntoThreads())
    }
    for (sh <- allSchedulingHandlersNotSCC) {
      sh.runner = runner
    }
  }

  private[this] var propagating = false

  def isPropagating: Boolean = propagating

  def triggerPropagation(upTo: PropagationElement): Unit = {
    if (!propagating) {
      propagating = true
      runner.run(upTo)
      propagating = false
    }
  }
}


/**
  * This is a O(1) dictionary for propagation elements.
  * It is based on an array, and the keys it support is only the PE that have been reistered
  * to the propagation structure by the time this is instantiated.
  * WARNING: this is not efficient if you do not actually use many of the keys
  * because the instantiated array will be very large compared to your benefits.
  * This might kill cache and RAM for nothing
  *
  * @param MaxNodeID the maximal ID of a node to be stored in the dictionary (since it is O(1) it is an array, and we allocate the full necessary size
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