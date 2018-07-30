package oscar.cbls.core.draft.propagation

import oscar.cbls.algo.quick.QList

/**
  * @param clusteredPEs are PE except the ones that are in a SCC, instead the SCC are in the PEs.
  * @param nbCLusteredPEs
  * @param noCycle
  */
class LayerSorterAlgo(propagationElementsNotInSCC:QList[PropagationElement]) {

  def sortNodesByLayer(): (Array[Int],Array[QList[PropagationElement]]) = {

    var nextFront: QList[PropagationElement] = null

    var currentFront: QList[PropagationElement] = null
    var nbPENotInSCC = 0

    for (pe <- propagationElementsNotInSCC) {
      nbPENotInSCC += 1
      if (!setLayerToPrecedingCount(pe)) {
        currentFront = QList(pe, currentFront)
      }
    }

    var currentLayer = 0 //la couche courante
    var count = 0 //the number of PE

    var countInCurrentLayer = 0
    var peInCurrentLayer: QList[PropagationElement] = null

    var stackOfLayerAndPEs: QList[(Int, QList[PropagationElement])] = null

    while (currentFront != null) {

      while (currentFront != null) {
        val pe = currentFront.head
        currentFront = currentFront.tail

        pe.propagationPosition = currentLayer

        countInCurrentLayer = countInCurrentLayer + 1
        peInCurrentLayer = QList(pe, peInCurrentLayer)

        nextFront = decrementSuccessorsNotInSCCAndAccumulateToFrontIfReachesZero(pe, nextFront)
      }

      currentFront = nextFront
      nextFront = null

      count = count + countInCurrentLayer

      stackOfLayerAndPEs = QList((countInCurrentLayer, peInCurrentLayer), stackOfLayerAndPEs)
      countInCurrentLayer = 0
      peInCurrentLayer = null
      currentLayer += 1
    }

    //finished iteration
    require(count == nbPENotInSCC,"internal bug")

    //composing arrays to return
    val layerToNbPes = Array.fill(currentLayer)(0)
    val layerToPes = Array.fill[QList[PropagationElement]](currentLayer)(null)

    var i = currentLayer
    while (i != 0) {
      i = i - 1
      layerToNbPes(i) = stackOfLayerAndPEs.head._1
      layerToPes(i) = stackOfLayerAndPEs.head._2
      stackOfLayerAndPEs = stackOfLayerAndPEs.tail
    }
    require(stackOfLayerAndPEs == null)

    (layerToNbPes, layerToPes)
  }

  def peInSCC(pe:PropagationElement):Boolean = {
    val s = pe.schedulingHandler
    s != null && s.isSCC
  }

  def nbPENotInSCC(pes:Iterable[PropagationElement]):Int = pes.count(!peInSCC(_))

  /**
    * Sets the layer of the PE to the number of other PE that must be scheduled before this one.
    * for scc, set it to the number of element that are referenced from other components or from the global SCC
    * @return true if there is at least one predecessor, false otherwise
    */
  private def setLayerToPrecedingCount(pe:PropagationElement): Boolean = {
    pe.propagationPosition = nbPENotInSCC(pe.staticallyListenedElements)
    pe.propagationPosition != 0
  }

  private def decrementSuccessorsNotInSCCAndAccumulateToFrontIfReachesZero(pe:PropagationElement,acc:QList[PropagationElement]):QList[PropagationElement] = {
    var toReturn = acc
    for (succeeding <- pe.staticallyListeningElements if !peInSCC(succeeding)) {
      //not in the same SCC as us
      toReturn = decrementLayerAndAccumulateIfReachesZero(succeeding,toReturn)
    }
    toReturn
  }

  private def decrementLayerAndAccumulateIfReachesZero(pe:PropagationElement,acc: QList[PropagationElement]): QList[PropagationElement] = {
    pe.propagationPosition -= 1
    if (pe.propagationPosition == 0) {
      QList(pe,acc)
    } else {
      acc
    }
  }
}

