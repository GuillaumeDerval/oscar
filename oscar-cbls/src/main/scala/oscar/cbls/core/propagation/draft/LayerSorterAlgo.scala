package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

/**
  *
  * @param clusteredPEs are PE except the ones that are in a SCC, instead the SCC are in the PEs.
  * @param nbCLusteredPEs
  * @param noCycle
  */
class LayerSorterAlgo(clusteredPEs:QList[PropagationElement],nbCLusteredPEs:Int,noCycle:Boolean) {

  def sortNodesByLayer(): (Array[Int],Array[QList[PropagationElement]]) = {

    var currentFront: QList[PropagationElement] = null
    var nextFront: QList[PropagationElement] = null

    for (pe <- clusteredPEs) {
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

        pe.layer = currentLayer

        countInCurrentLayer = countInCurrentLayer + 1
        peInCurrentLayer = QList(pe, peInCurrentLayer)

        nextFront = decrementSuccessorsAndAccumulateFrontIfReachesZero(pe, nextFront)
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
    if (count != nbCLusteredPEs) {
      if (noCycle) {
        throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
      } else {
        throw new Exception("internal bug")
      }
    }

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

  /**
    * Sets the layer of the PE to the number of other PE that must be scheduled before this one.
    * for scc, set it to the number of element that are referenced from other components or from the global SCC
    * @return true if there is at least one predecessor, false otherwise
    */
  private def setLayerToPrecedingCount(pe:PropagationElement): Boolean = {
    pe match{
      case scc:StronglyConnectedComponent =>
        scc.layer = scc.propagationElements.count(p => setLayerToPrecedingCount(p))
      case _ =>
        //le compteur est mis au nombre de noeud precedent qui ne sont pas dans la meme composante connexe ou qui sont dans aucune composante connexe
        val scc = pe.scc
        if(scc == null){
          pe.layer = pe.staticallyListenedElements.size
        }else{
          //this is in a SCC, called through the recursive call here above
          pe.staticallyListenedElements.count(p => p.scc != scc)
        }
    }
    pe.layer != 0
  }

  private def decrementSuccessorsAndAccumulateFrontIfReachesZero(pe:PropagationElement,acc:QList[PropagationElement]):QList[PropagationElement] = {
    pe match{
      case scc:StronglyConnectedComponent =>
        var toReturn = acc
        for (pe2 <- scc.propagationElements) {
          for(peSucc <- pe2.staticallyListeningElements){
            if(peSucc != scc) {
              toReturn = decrementLayerAndAccumulateIfReachesZero(peSucc, toReturn)
            }
          }
        }
        toReturn
      case _ =>
        var toReturn = acc
        for (succeeding <- pe.staticallyListeningElements) {
          //not in the same SCC as us
          toReturn = decrementLayerAndAccumulateIfReachesZero(succeeding,toReturn)
        }
        toReturn
    }
  }

  private def decrementLayerAndAccumulateIfReachesZero(pe:PropagationElement,acc: QList[PropagationElement]): QList[PropagationElement] = {
    require(!pe.isInstanceOf[StronglyConnectedComponent])
    pe.layer -= 1
    if (pe.layer == 0) {
      //faut pusher qqchose
      if(pe.scc == null){
        //not in a SCC, so we push PE
        QList(pe,acc)
      }else{
        //in a SCC, so we decrease the layer of the SCC itself
        decrementLayerAndAccumulateIfReachesZero(pe.scc,acc)
      }
    } else {
      acc
    }
  }
}
