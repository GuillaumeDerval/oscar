package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.tarjan.{TarjanNodeData, TarjanWithExternalStorage}

class SCCIdentifierAlgo(pEs:QList[PropagationElement],ps:PropagationStructure) {

  def identifySCC(): (QList[PropagationElement], QList[StronglyConnectedComponent]) = {
    val storageForTarjan = ps.buildNodeStorage[TarjanNodeData]
    storageForTarjan.initialize(() => new TarjanNodeData)

    val stronglyConnectedComponents: List[QList[PropagationElement]] = TarjanWithExternalStorage.getStronglyConnexComponents[PropagationElement](
      ps.allPropagationElements,
      p => p.staticallyListeningElements,
      storageForTarjan.get)

    var acyclic = true
    var stronglyConnectedComponentsStructures: QList[StronglyConnectedComponent] = null
    var clusteredPropagationElements: QList[PropagationElement] = null

    for (pEOfSCC <- stronglyConnectedComponents) {
      if (pEOfSCC.tail == null) {
        //only one PE, so not in a SCC
        clusteredPropagationElements = QList(pEOfSCC.head, clusteredPropagationElements)
      } else {
        //a new SCC must be instantiated here
        acyclic = false
        val scc = new StronglyConnectedComponent(pEOfSCC, pEOfSCC.size, ps)
        stronglyConnectedComponentsStructures = QList(scc, stronglyConnectedComponentsStructures)
        clusteredPropagationElements = QList(scc, clusteredPropagationElements)
      }
    }

    (clusteredPropagationElements, stronglyConnectedComponentsStructures)
  }
}

