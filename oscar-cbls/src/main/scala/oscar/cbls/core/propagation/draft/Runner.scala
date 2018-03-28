package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

abstract class Runner() {

  final def enqueue(pes: QList[PropagationElement]) {
    this.synchronized {
      var remainingPes = pes
      while(remainingPes != null){
        val pe = remainingPes.head
        remainingPes = remainingPes.tail

        enqueuePENS(pe)
      }
    }
  }

  final def enqueuePE(pe:PropagationElement) {
    this.synchronized {
      enqueuePENS(pe)
    }
  }

  protected def enqueuePENS(pe:PropagationElement)

  def run(upTo:PropagationElement)
}

class NaiveMonoThreadRunner(nbPe:Int) extends Runner{

  private [this] val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.layer, nbPe)

  override protected def enqueuePENS(pe: PropagationElement){
    h.insert(pe)
  }

  def run(upTo:PropagationElement): Unit ={
    //this runner does not care about upTo, it propagates everything.
    require(upTo.schedulingHandler.runner == this)
    upTo.schedulingHandler.enqueueForRun()

    while (!h.isEmpty) {
      val x:PropagationElement = h.popFirst()
      x.performPropagation() //through the schedulingHandler, other PE are enqueued.
    }
    upTo.schedulingHandler.notifyEndRun()
  }
}

class MonoThreadRunner(nbLayers:Int) extends Runner(){

  override protected def enqueuePENS(pe: PropagationElement){
    val layer = pe.layer
    if(layersToPEs(layer) == null){
      nonEmptyLayers.insert(layer)
    }
    layersToPEs(layer) = QList(pe, layersToPEs(layer))
  }

  private[this] val layersToPEs: Array[QList[PropagationElement]] = Array.fill(nbLayers)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  override def run(upTo:PropagationElement) {
    require(upTo.schedulingHandler.runner == this)
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

    upTo.schedulingHandler.notifyEndRun()
  }
}

