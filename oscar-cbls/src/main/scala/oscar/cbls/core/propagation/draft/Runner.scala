package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

abstract class Runner() {
  def enqueue(pes: QList[PropagationElement])

 def performPropagation(upTo:PropagationElement)
}

class MonoThreadRunner(nbLayers:Int) extends Runner(){

 override def enqueue(pes: QList[PropagationElement]) {
    nonEmptyLayers.synchronized {
      for (pe <- pes) {
        val layer = pe.layer
        if(layersToPEs(layer) == null){
          nonEmptyLayers.insert(layer)
        }
        layersToPEs(layer) = QList(pe, layersToPEs(layer))
      }
    }
  }

  private[this] val layersToPEs: Array[QList[PropagationElement]] = Array.fill(nbLayers)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  override def performPropagation(upTo:PropagationElement) {
    upTo.schedulingHandler.enqueueForRun()

    while(nonEmptyLayers.nonEmpty) {
      val currentLayer = nonEmptyLayers.popFirst()

      var toPropagate: QList[PropagationElement] = layersToPEs(currentLayer)
      layersToPEs(currentLayer) = null

      while (toPropagate != null) {
        toPropagate.head.performPropagation()
        toPropagate = toPropagate.tail
      }
    }
  }
}
