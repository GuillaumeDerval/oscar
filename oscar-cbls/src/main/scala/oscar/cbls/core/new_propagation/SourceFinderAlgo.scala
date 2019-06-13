package oscar.cbls.core.new_propagation

import oscar.cbls.algo.quick.QList

class SourceFinderAlgo(propagationStructure:PropagationStructure) {

  val marking = propagationStructure.buildFilledNodeStorage[Boolean](false)
  var toUnmark: QList[PropagationElement] = _ // null

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
    exploreAndReturn(false, pes:_*)
  }

  //returns the set of source variable that define this one.
  // This exploration procedure explores passed dynamic invariants,
  // but it over-estimates the set of source variables over dynamic invariants, as it uses the static graph.
  def getSourcesAndAllIntermediary(pes: PropagationElement*): QList[PropagationElement] = {
    exploreAndReturn(true, pes:_*)
  }

  private def exploreAndReturn(allIntermediary:Boolean, pes: PropagationElement*):QList[PropagationElement] = {
    unMarkAll()

    var toExplore: QList[PropagationElement] = null
    var toReturn: QList[PropagationElement] = null

    for (pe <- pes) {
      if (!markAndReturnWasMarked(pe)) {
        //new toExplore
        toExplore = QList(pe, toExplore)
        if (allIntermediary) toReturn = QList(pe, toReturn)
      }
    }

    while (toExplore != null) {
      val currentPE = toExplore.head
      toExplore = toExplore.tail
      var l = currentPE.staticallyListeningElements

      if (!allIntermediary && l == null) {
        toReturn = QList(currentPE, toReturn)
      }

      while (l != null) {
        val newPE = l.head
        l = l.tail
        if (!markAndReturnWasMarked(newPE)) {
          //new toExplore
          toExplore = QList(newPE)
          if (allIntermediary) toReturn = QList(currentPE, toReturn)
        }
      }
    }
    toReturn
  }
}

