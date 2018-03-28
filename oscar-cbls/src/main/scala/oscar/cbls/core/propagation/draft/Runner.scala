package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

abstract class Runner() {
  def enqueue(pes: QList[PropagationElement]){
    var digest = pes
    while (digest != null) {
      enqueuePE(digest.head)
      digest = digest.tail
    }
  }

  def enqueuePE(pe:PropagationElement)

  def performPropagation(upTo:PropagationElement)
}

class NaiveMonoThreadRunner(nbPe:Int) extends Runner{

  private [this] val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.layer, nbPe)

  override def enqueue(pes: QList[PropagationElement]): Unit ={
    synchronized(h) {
      var digest = pes
      while (digest != null) {
        h.insert(digest.head)
        digest = digest.tail
      }
    }
  }

  override def enqueuePE(pe:PropagationElement){
    synchronized(h) {
      h.insert(pe)
    }
  }

  def performPropagation(upTo:PropagationElement): Unit ={
    //this runner does not care about upTo, it propagates everything.
    require(upTo.schedulingHandler.runner == this)
    upTo.schedulingHandler.enqueueForRun()

    while (!h.isEmpty) {
      var x:PropagationElement = h.popFirst()
      x.performPropagation() //through the schedulingHandler, other PE are enqueued.
    }
    upTo.schedulingHandler.notifyEndRun()
  }
}

class MonoThreadRunner(nbLayers:Int) extends Runner(){

  override def enqueue(pes: QList[PropagationElement]) {
    nonEmptyLayers.synchronized {
      var digest = pes
      while(pes != null){
        val pe = digest.head
        val layer = pe.layer

        digest = digest.tail
        if(layersToPEs(layer) == null){
          nonEmptyLayers.insert(layer)
        }
        layersToPEs(layer) = QList(pe, layersToPEs(layer))
      }
    }
  }

  override def enqueuePE(pe: PropagationElement){
    nonEmptyLayers.synchronized {
      val layer = pe.layer
      if(layersToPEs(layer) == null){
        nonEmptyLayers.insert(layer)
      }
      layersToPEs(layer) = QList(pe, layersToPEs(layer))
    }
  }

  private[this] val layersToPEs: Array[QList[PropagationElement]] = Array.fill(nbLayers)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  override def performPropagation(upTo:PropagationElement) {
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

