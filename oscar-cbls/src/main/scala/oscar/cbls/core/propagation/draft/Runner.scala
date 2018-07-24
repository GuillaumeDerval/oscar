package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

abstract class Runner() {

  final def enqueue(pes: QList[PropagationElement]) {
    var remainingPes = pes
    while (remainingPes != null) {
      val pe = remainingPes.head
      remainingPes = remainingPes.tail
      enqueuePE(pe)
    }
  }

  @inline
  def enqueuePE(pe:PropagationElement)

  final def run(upTo:PropagationElement): Unit ={
    runSH(upTo.schedulingHandler)
  }

  final def runSH(upTo:SimpleSchedulingHandler) {
    require(upTo.runner == this)
    upTo.enqueueForRun()

    doRun()

    upTo.notifyEndRun()
  }

  protected def doRun():Unit
}

//runs based on the position: smallest first
class TotalOrderRunner(nbPe:Int) extends Runner(){

  private [this] val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.positionInTopologicalSort, nbPe)

  @inline
  override protected def enqueuePE(pe: PropagationElement){
    h.insert(pe)
  }

  override protected def doRun(): Unit ={
    while (!h.isEmpty) {
      val x:PropagationElement = h.popFirst()
      x.propagate() //through the schedulingHandler, other PE are enqueued.
    }
  }
}

class LayerSortRunner(nbLayers:Int) extends Runner(){

  @inline
  override protected def enqueuePE(pe: PropagationElement){
    val layer = pe.propagationPosition
    val pEOfLayer = layersToPEs(layer)
    if(pEOfLayer == null) {
      nonEmptyLayers.insert(layer)
    }
    layersToPEs(layer) = QList(pe, pEOfLayer)
  }

  private[this] val layersToPEs: Array[QList[PropagationElement]] = Array.fill(nbLayers)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  override protected def doRun(): Unit ={
    while(nonEmptyLayers.nonEmpty) {
      val currentLayer = nonEmptyLayers.popFirst()

      var toPropagate: QList[PropagationElement] = layersToPEs(currentLayer)
      layersToPEs(currentLayer) = null

      while (toPropagate != null) {
        toPropagate.head.propagate()
        toPropagate = toPropagate.tail
      }
    }
  }
}
