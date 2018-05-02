package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

abstract class Runner(threadSafe:Boolean) {

  final def enqueue(pes: QList[PropagationElement]) {
    if(threadSafe) {
      this.synchronized {
        myEnqueue(pes)
      }
    }else{
      myEnqueue(pes)
    }
  }

  private def myEnqueue(pes: QList[PropagationElement]) {
    var remainingPes = pes
    while (remainingPes != null) {
      val pe = remainingPes.head
      remainingPes = remainingPes.tail

      enqueuePENS(pe)
    }
  }

  final def enqueuePE(pe:PropagationElement) {
    if(threadSafe) {
      this.synchronized {
        enqueuePENS(pe)
      }
    }else{
      enqueuePENS(pe)
    }
  }

  @inline
  protected def enqueuePENS(pe:PropagationElement)

  final def run(upTo:PropagationElement): Unit ={
    runSH(upTo.schedulingHandler)
  }

  def runSH(upTo:SimpleSchedulingHandler)
}

class TotalOrderRunner(nbPe:Int, threadSafe:Boolean) extends Runner(threadSafe){

  private [this] val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.layer, nbPe)

  @inline
  override protected def enqueuePENS(pe: PropagationElement){
    h.insert(pe)
  }

  override def runSH(upTo:SimpleSchedulingHandler): Unit ={
    //this runner does not care about upTo, it propagates everything.
    require(upTo.runner == this)
    upTo.enqueueForRun()

    while (!h.isEmpty) {
      val x:PropagationElement = h.popFirst()
      x.propagate() //through the schedulingHandler, other PE are enqueued.
    }
    upTo.notifyEndRun()
  }
}

class LayerSortRunner(nbLayers:Int, threadSafe:Boolean) extends Runner(threadSafe){

  @inline
  override protected def enqueuePENS(pe: PropagationElement){
    val layer = pe.layer
    if(layersToPEs(layer) == null){
      nonEmptyLayers.insert(layer)
    }
    layersToPEs(layer) = QList(pe, layersToPEs(layer))
  }

  private[this] val layersToPEs: Array[QList[PropagationElement]] = Array.fill(nbLayers)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  override def runSH(upTo:SimpleSchedulingHandler) {
    require(upTo.runner == this)
    upTo.enqueueForRun()

    while(nonEmptyLayers.nonEmpty) {
      val currentLayer = nonEmptyLayers.popFirst()

      var toPropagate: QList[PropagationElement] = layersToPEs(currentLayer)
      layersToPEs(currentLayer) = null

      while (toPropagate != null) {
        toPropagate.head.propagate()
        toPropagate = toPropagate.tail
      }
    }

    upTo.notifyEndRun()
  }
}

