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

  def doRun():Unit
}

//runs based on the position: smallest first
class TotalOrderRunner(nbPe:Int) extends Runner(){

  private [this] val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.positionInTopologicalSort, nbPe)

  @inline
  override def enqueuePE(pe: PropagationElement){
    h.insert(pe)
  }

  override def doRun(): Unit ={
    while (!h.isEmpty) {
      val x:PropagationElement = h.popFirst()
      x.propagate() //through the schedulingHandler, other PE are enqueued.
    }
  }
}

class LayerSortRunner()
  extends Runner(){

  private[this] var layersToPEs: Array[QList[PropagationElement]] = _
  private[this] var nonEmptyLayers: BinomialHeap[Int] = _

  private var myNbLayer:Int = 0

  def nbLayer:Int = myNbLayer
  def nbLayer_=(n:Int) = {
    require(nonEmptyLayers.isEmpty)
    myNbLayer = n

    layersToPEs = Array.fill(myNbLayer)(null)
    nonEmptyLayers = new BinomialHeap[Int]((item: Int) => item, myNbLayer)
  }

  @inline
  override def enqueuePE(pe: PropagationElement){
    val layer = pe.propagationPosition
    val pEOfLayer = layersToPEs(layer)
    if(pEOfLayer == null) {
      nonEmptyLayers.insert(layer)
    }
    layersToPEs(layer) = QList(pe, pEOfLayer)
  }


  override def doRun(): Unit ={
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
