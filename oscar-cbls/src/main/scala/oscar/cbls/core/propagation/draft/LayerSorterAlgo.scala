package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.algo.quick.QList

class LayerSorterAlgo(pes:QList[PropagationElement]) {

  def sortNodesByLayer(): (Array[Int],Array[QList[PropagationElement]]) = {

  }


  /**
    * This computes the position of the clustered PE based on distance to input,
    * that is: the SCC and the PE not belonging to an SCC
    * @return the max Position, knowing that the first is zero
    */
  def computePositionsThroughDistanceToInput(ClusteredPropagationComponents: List[PropagationElement]): Int = {
    val front: DoublyLinkedList[PropagationElement] = new DoublyLinkedList[PropagationElement]()
    for (pe <- ClusteredPropagationComponents) {
      pe.setCounterToPrecedingCount()
      if (pe.position == 0) front.enqueue(pe)
    }
    front.enqueue(null) //null marker denotes when Position counter must be incremented
    var position = 0 //la position du prochain noeud place.
    var count = 0 //the number of PE
    var countInLayer = 0

    while (true) {
      //we know it is not empty here
      val n = front.dequeue()
      if (n == null) {
        if (front.isEmpty) {
          if (count != ClusteredPropagationComponents.size) {
            if (noCycle) {
              throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
            } else {
              throw new Exception("internal bug")
            }
          }
          return position + 1
        } else {
          countInLayer = 0
          position += 1
          front.enqueue(null) //null marker denotes when Position counter must be incremented
        }
      } else {
        n.position = position
        count += 1
        countInLayer += 0
        for (pe <- n.decrementSucceedingAndAccumulateFront(List.empty)) front.enqueue(pe)
      }
    }
    0 //never reached
  }

}
