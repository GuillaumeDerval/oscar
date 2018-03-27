package oscar.cbls.core.propagation.draft


import PropagationImpactCharacteristics._
import oscar.cbls.algo.quick.QList

class PropagationStructure(nbSystemThread:Int) extends SchedulingHandler {

  var allSchedulingHandlers: QList[SchedulingHandler] = null

  def registerSchedulingHandler(s: SchedulingHandler): Unit = {
    allSchedulingHandlers = QList(s, allSchedulingHandlers)
  }

  var allPropagationElements: QList[PropagationElement]
  var layerToPropagationElements: Array[QList[PropagationElement]] = null
  var layerToNbPropagationElements: Array[Int] = null

  //can only be called when all SH are created
  override def runner_=(runner: Runner) {
    super.runner_=(runner)
    for (s <- allSchedulingHandlers) {
      s.runner = runner
    }
  }


  def registerForPartialPropagation(pe: PropagationElement): Unit = {
    //we root a schedulingHandler at this node.
  }

  private var nbLayer: Int = -1
  private var runner: Runner = null


  def close(): Unit = {
    identifySCC()
    instantiateVSH()

    (layerToPropagationElements,layerToNbPropagationElements)= new LayerSorterAlgo(allPropagationElements).sortNodesByLayer()


    //perform the multi-threaded analysis
    partitionGraphIntoSchedulingHandlers()

    //create runner and multtreaded parttion (if multi-treading)
    runner = if (nbSystemThread == 1) {
      new MonoThreadRunner(nbLayer)
    } else {
      new MultiThreadRunner(nbSystemThread, new MultiTreadingPartitioningAlgo(layerToPropagationElements, layerToNbPropagationElements).partitionGraphIntoThreads())
    }
    for (sh <- allSchedulingHandlers) {
      sh.runner = runner
    }
  }




  def partitionGraphIntoSchedulingHandlers(): Unit = {

    for (pe <- propagationElements) {
      allPropagationElements
    }
  }

  private[this] var propagating = false

  def isPropagating: Boolean = propagating

  def triggerPropagation(upTo: PropagationElement): Unit = {
    if (!propagating) {
      propagating = true
      runner.performPropagation(upTo)
      propagating = false
    }
  }
}


