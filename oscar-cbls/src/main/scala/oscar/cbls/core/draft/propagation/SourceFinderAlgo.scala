package oscar.cbls.core.draft.propagation

import oscar.cbls.algo.quick.QList

class SourceFinderAlgo(propagationStructure:PropagationStructure) {

  val marking = propagationStructure.buildNodeStorage[Boolean](false)
  var toUnmark: QList[PropagationElement] = null

  def markAndReturnWasMarked(pe: PropagationElement): Boolean = {
    if (marking(pe)) {
      true
    } else {
      toUnmark = QList(pe, toUnmark)
      marking(pe) = true
      false
    }
  }

  def unMarkAll(): Unit = {
    while (toUnmark != null) {
      marking(toUnmark.head) = false
      toUnmark = toUnmark.tail
    }
  }

  //returns the set of source variable that define this one.
  // This exploration procedure explores passed dynamic invariants,
  // but it over-estimates the set of source variables over dynamic invariants, as it uses the static graph.
  def getSources(pes: PropagationElement*): QList[PropagationElement] = {
    unMarkAll()

    var toExplore: QList[PropagationElement] = null

    for (pe <- pes) {
      toExplore = QList(pe, toExplore)
    }

    var toReturn: QList[PropagationElement] = null

    while (toExplore != null) {
      val currentPE = toExplore.head
      toExplore = toExplore.tail

      if (currentPE.staticallyListeningElements == null) {
        toReturn = QList(currentPE, toReturn)
      } else {
        var l = currentPE.staticallyListeningElements
        while (l != null) {
          val newPE = l.head
          l = l.tail
          if (!markAndReturnWasMarked(newPE)) {
            //new toExplore
            toExplore = QList(newPE)
          }
        }
      }
    }
    toReturn
  }
}

