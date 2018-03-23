package oscar.cbls.core.propagation.draft

import java.util.concurrent.Executors

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

object Runner {

  def apply(nbLayer:Int,nbThreadOPtDefaultMax:Option[Int]):Runner = {
    val nbThread = nbThreadOPtDefaultMax match{
      case None => Runtime.getRuntime.availableProcessors()
      case Some(x) => x
    }
    if (nbThread == 1) new MonoThreadRunner(nbLayer)
    else  new MultiThreadRunner(nbThread, nbLayer)
  }
}

abstract class Runner(nbLayers:Int) {
  def enqueue(pes: QList[PropagationElement])

 def performPropagation(upTo:PropagationElement)
}

class MultiThreadRunner(nbThread:Int, nbLayers:Int) extends Runner(nbLayers:Int){

  override def enqueue(pes: QList[PropagationElement]) {
    nonEmptyLayers.synchronized {
      var remainingPes = pes
      while(remainingPes != null){
        val pe = remainingPes.head
        remainingPes = remainingPes.tail
        val finalArray = threadToLayersToPEs(pe.threadID)
        val layer = pe.layer
        finalArray(layer) = QList(pe, finalArray(layer))
        if(!layerToAnyPe(layer)){
          nonEmptyLayers.insert(layer)
          layerToAnyPe(layer) = true
        }
      }
    }
  }

  private[this] val threadToLayersToPEs: Array[Array[QList[PropagationElement]]] = Array.tabulate(nbThread)(t => Array.fill(nbLayers)(null))
  private[this] val layerToAnyPe: Array[Boolean] = Array.fill(nbLayers)(false)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayers)

  private[this] val threadPool = Executors.newFixedThreadPool(nbThread)
  private[this] val threadIds = 0 until nbThread

  override def performPropagation(upTo:PropagationElement) {

    upTo.schedulingHandler.enqueueForRun()
    val toPropagateArray:Array[QList[PropagationElement]] = Array.fill(nbThread)(null)

    while(nonEmptyLayers.nonEmpty) {
      //we fetch the stuff to propagate at this layer in a non-parallelized way
      //because propagation itself might insert more stuff to propagate
      val currentLayer = nonEmptyLayers.popFirst()
      layerToAnyPe(currentLayer) = false

      var threadID = nbThread
      while(threadID!=0){
        threadID = threadID -1
        val myTrack = threadToLayersToPEs(threadID)
        toPropagateArray(threadID) = myTrack(currentLayer)
        myTrack(currentLayer) = null
      }

      //multi-treading starts here
      //Start the propagation for each thread
      val futures = threadIds.map(threadID => {
        var myPEs = toPropagateArray(threadID)
        threadPool.submit(new Runnable {
          override def run(): Unit = {
            while (myPEs != null) {
              myPEs.head.performPropagation()
              myPEs = myPEs.tail
            }
          }
        })
      })

      //wait for each task to complete
      futures.foreach(_.get())
      //multi-treading ends here

    }
  }
}

class MonoThreadRunner(nbLayers:Int) extends Runner(nbLayers:Int){

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
