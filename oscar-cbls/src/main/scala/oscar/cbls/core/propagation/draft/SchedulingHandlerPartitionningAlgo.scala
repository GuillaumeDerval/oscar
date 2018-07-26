package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.tarjan.{TarjanNodeData, TarjanWithExternalStorage}

import scala.collection.immutable.SortedSet

class SchedulingHandlerPartitioningAlgo(p:PropagationStructure) {

  def instantiateVariableSchedulingHandlersForPENotInSCC(): Unit = {
    for (pe <- p.allPropagationElements if pe.scc.isDefined) {
      pe match {
        case pev: PropagationElement with VaryingDependencies =>
          val dsh = new SchedulingHandlerForPEWithVaryingDependencies(pev, p)
      }
    }
  }

  def identifyAndInstantiateSCC(): (QList[PropagationElement], QList[StronglyConnectedComponent]) = {
    val storageForTarjan = p.buildNodeStorage[TarjanNodeData]
    storageForTarjan.initialize(() => new TarjanNodeData)

    val stronglyConnectedComponents: List[QList[PropagationElement]] =
      TarjanWithExternalStorage.getStronglyConnexComponents[PropagationElement](
        p.allPropagationElements,
        p => p.staticallyListeningElements,
        storageForTarjan.get)

    var acyclic = true
    var stronglyConnectedComponentsStructures: QList[StronglyConnectedComponent] = null
    var propagationElementsNotInSCC: QList[PropagationElement] = null

    for (pEOfSCC <- stronglyConnectedComponents) {
      if (pEOfSCC.tail == null) {
        //only one PE, so not in a SCC
        propagationElementsNotInSCC = QList(pEOfSCC.head, propagationElementsNotInSCC)
      } else {
        //a new SCC must be instantiated here
        acyclic = false
        val scc = new StronglyConnectedComponent(pEOfSCC, pEOfSCC.size, p)
        stronglyConnectedComponentsStructures = QList(scc, stronglyConnectedComponentsStructures)

        //we also add the call back PE of the SCC
        propagationElementsNotInSCC = QList(scc.myCallBackPE, propagationElementsNotInSCC)
      }
    }

    (propagationElementsNotInSCC, stronglyConnectedComponentsStructures)
  }

  //graph must already be sorted by layers, have SCC and vSH instantiated
  def partitionGraphIntoSchedulingHandlers() {
    var currentLayerID = p.layerToClusteredPropagationElements.length
    while (currentLayerID > 0) {
      currentLayerID = currentLayerID - 1

      //from outputs to inputs decorate layer by layer
      //the SH of a node is the SH of its successors
      //except if the node already has a sh or if its successors have different sh's or if the SH of its successor is a VSH or if the node has no successor at all
      //in this case, a new SH is instantiated

      for (pe <- p.layerToClusteredPropagationElements(currentLayerID) if pe.schedulingHandler == null) {
        //we need to set a scheduling handler to this node

        var staticallyListeningElements = pe.staticallyListeningElements

        if (staticallyListeningElements == null) {
          //it has no successor, so it gets a new scheduling handler and job is done.

          pe.schedulingHandler = new SimpleSchedulingHandler(p)

        } else {
          //it has some successor, so we need to check if they all have the same scheduling handler
          //if
          val referenceListeningSchedulingHandler = null
          var newSHNeededSoFar = false
          while (staticallyListeningElements != null && !newSHNeededSoFar) {

            staticallyListeningElements.head.schedulingHandler match {
              case scc: StronglyConnectedComponent =>
                newSHNeededSoFar = true //we add a new SH
              case simple: SimpleSchedulingHandler =>
                if (referenceListeningSchedulingHandler == null) {
                  referenceListeningSchedulingHandler == simple
                  staticallyListeningElements = staticallyListeningElements.tail
                } else if (referenceListeningSchedulingHandler != simple) {
                  //it already has a SH, and it is another one, so we need a new SH
                  newSHNeededSoFar = true //we add a new SH
                } else {
                  staticallyListeningElements = staticallyListeningElements.tail
                }
              case dyn: SchedulingHandlerForPEWithVaryingDependencies =>
                newSHNeededSoFar = true //we add a new SH
            }
          }

          if (newSHNeededSoFar) {
            val newSchedulingHandler = new SimpleSchedulingHandler(p)
            pe.schedulingHandler = newSchedulingHandler

            var allListeningSH: QList[SchedulingHandler] = null

            def addListeningSH(sh: SchedulingHandler) {
              if (!sh.isRunning) {
                sh.isRunning = true
                allListeningSH = QList(sh, allListeningSH)
                newSchedulingHandler.addListeningSchedulingHandler(sh)
              }
            }

            addListeningSH(referenceListeningSchedulingHandler)

            while (staticallyListeningElements != null) {
              addListeningSH(staticallyListeningElements.head.schedulingHandler)
              staticallyListeningElements = staticallyListeningElements.tail
            }

            while (allListeningSH != null) {
              allListeningSH.head.isRunning = false
              allListeningSH = allListeningSH.tail
            }

          } else {
            pe.schedulingHandler = referenceListeningSchedulingHandler
          }

        }
      }
    }
  }
}

