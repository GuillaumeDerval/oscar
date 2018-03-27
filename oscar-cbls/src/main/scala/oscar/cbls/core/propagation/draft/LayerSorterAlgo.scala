package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.algo.quick.QList


class LayerSorterAlgo(clustredPes:QList[PropagationElement],nbCLusteredPEs:Int,noCycle:Boolean) {

  def sortNodesByLayer(): (Array[Int],Array[QList[PropagationElement]]) = {

    var currentFront: QList[PropagationElement] = null
    var nextFront: QList[PropagationElement] = null

    for (pe <- clustredPes) {
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
    * @return true if there is a predecesor, false otherwise
    */
  def setLayerToPrecedingCount(pe:PropagationElement): Boolean = {
    pe match{
      case scc:StronglyConnectedComponent =>
        scc.layer = scc.propagationElements.count(p => setLayerToPrecedingCount(p))
        scc.layer != 0
      case _ =>
        //le compteur est mis au nombre de noeud precedent qui ne sont pas dans la meme composante connexe
        pe.schedulingHandler match {
          case scc: StronglyConnectedComponent =>
            pe.layer = pe.staticallyListenedElement.count(p => p.schedulingHandler != scc && p.schedulingHandler != null)
          case ps: PropagationStructure =>
            pe.layer = pe.staticallyListenedElement.count(p => p.schedulingHandler != null)
        }
        pe.layer != 0
    }
  }


  def decrementSuccessorsAndAccumulateFrontIfReachesZero(pe:PropagationElement,acc:QList[PropagationElement]):QList[PropagationElement] = {
    pe match{
      case scc:StronglyConnectedComponent =>
        var toReturn = acc
        for (pe2 <- scc.propagationElements) {
          toReturn = decrementSuccessorsAndAccumulateFrontIfReachesZero(pe2,toReturn)
        }
        toReturn
      case _ =>
        var toreturn = acc
        for (succeeding <- pe.staticallyListeningElements) {
          if (succeeding.schedulingHandler == pe.mySchedulingHandler.propagationStructure || succeeding.schedulingHandler != mySchedulingHandler) {
            //not in the same SCC as us
            toreturn = decrementAndAccumulateFrontIfReachesZero(succeeding,toreturn)
          }
        }
        toreturn
    }
  }

  final def decrementAndAccumulateFrontIfReachesZero(pe:PropagationElement,acc: QList[PropagationElement]): QList[PropagationElement] = {
    pe.layer -= 1
    if (pe.layer == 0) {
      //faut pusher qqchose
      pe.scc match {
        case scc: StronglyConnectedComponent =>
          scc.decrementAndAccumulateFront(acc)
        case s: PropagationStructure => this :: acc
      }
    } else {
      acc
    }
  }




  /**
    * This computes the position of the clustered PE based on distance to input,
    * that is: the SCC and the PE not belonging to an SCC
    * @return the max Position, knowing that the first is zero
    */
  def computePositionsThroughDistanceToInput(ClusteredPropagationComponents: List[PropagationElement]): Int = {
  }

}
