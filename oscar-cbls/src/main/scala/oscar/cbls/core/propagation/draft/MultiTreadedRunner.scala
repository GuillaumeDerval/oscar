package oscar.cbls.core.propagation.draft

import java.util.concurrent.{Executors, Future}

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.propagation.draft.PropagationImpactCharacteristics._

class MultiTreadingPartitioningAlgo(layerToPropagationElements: Array[QList[PropagationElement]],
                                    layerToNbPropagationElements: Array[Int]){
  val nbLayer:Int = layerToNbPropagationElements.length


  //multi-treading partitioning algorithms

  /**
    * partitions the graph into treads, alocate threadsIDs to PEs.
    * PEs must already be partitioned into layers
    * their threadID must be -1 the default initial value
    * @return an array layer to nbThreads allocated
    */
  def partitionGraphIntoThreads():Array[Int] =  {
    //goal is that each PE gets a threadIT
    //each PE has its propagationImpactZone: a set of PE's that will be impacted when PE.propagate is called.
    //two PE on the same layer must have the same TreadID if there is an intersection in their propagationImpactZone

    var biggestTreadIdUsedSoFar: Int = -2
    Array.tabulate[Int](nbLayer)((layer:Int) => {
      val (arrayOfMarkedNodesAndInterferingThreadIDs:Array[(PropagationElement,QList[Int])], newBiggestTreadIdUsedSoFar:Int) = markImpactZoneOfLayer(layer,biggestTreadIdUsedSoFar)
      val nbTreadsForThisLayer = allocateThreadIDForLayer(arrayOfMarkedNodesAndInterferingThreadIDs,biggestTreadIdUsedSoFar)
      biggestTreadIdUsedSoFar = newBiggestTreadIdUsedSoFar
      nbTreadsForThisLayer
    })
  }

  /**
    *
    * @param arrayOfMarkedNodesAndInterferingThreadIDs
    * @param offsetForNodeMarking
    * @return the number of threads of this layer
    */
  def allocateThreadIDForLayer(arrayOfMarkedNodesAndInterferingThreadIDs:Array[(PropagationElement,QList[Int])],offsetForNodeMarking:Int): Int = {
    //we compute the threadIDs and store them into finalTreadID
    val finalTreadID = Array.fill[Int](arrayOfMarkedNodesAndInterferingThreadIDs.length)(-1)
    var finalTreadIDCounter: Int = -1
    var i = arrayOfMarkedNodesAndInterferingThreadIDs.length
    while (i > 0) {
      i = i - 1
      val pe = arrayOfMarkedNodesAndInterferingThreadIDs(i)._1
      require(i == offsetForNodeMarking - pe.threadID)

      if(pe.couldBePropagated){
        if (finalTreadID(i) == -1) {
          //no final thead ID, so we can allocate a new one
          finalTreadIDCounter += 1
          finalTreadID(i) = finalTreadIDCounter
        }
        val finalThreadIDForCurrentPEAndInterferingPEs = finalTreadID(i)
        //propagate to interfering Pes
        for (interferingThreadID <- arrayOfMarkedNodesAndInterferingThreadIDs(i)._2) {
          require(interferingThreadID == arrayOfMarkedNodesAndInterferingThreadIDs(offsetForNodeMarking - interferingThreadID)._1.threadID)
          finalTreadID(offsetForNodeMarking - pe.threadID) = finalThreadIDForCurrentPEAndInterferingPEs
        }
      }else{
        //could not be propagated
        finalTreadID(i) = -1 //We stuff an erroneous value here.
      }
    }

    //assign threadIDs from finalTreadID
    i = arrayOfMarkedNodesAndInterferingThreadIDs.length
    while (i > 0) {
      i = i - 1
      arrayOfMarkedNodesAndInterferingThreadIDs(i)._1.threadID = finalTreadID(i)
    }

    //return the number of threads that are allocate on this layer
    finalTreadIDCounter+1
  }

  def markImpactZoneOfLayer(layer:Int,biggestTreadIdUsedSoFar:Int):(Array[(PropagationElement,QList[Int])],Int) = {

    var currentPEIDInCurrentLayer = 0
    val nbPEInCurrentLayer = layerToNbPropagationElements(layer)
    val arrayOfPEAndInterferingThreadIDs = Array.fill[(PropagationElement,QList[Int])](nbPEInCurrentLayer)(null)
    var currentThreadIDForNodeMarking = biggestTreadIdUsedSoFar

    for (pe <- layerToPropagationElements(layer)) {
      //We want to know about the nodes that will perform propagation, and notify on propagation to mark their notification

      currentThreadIDForNodeMarking = currentThreadIDForNodeMarking - 1

      val shouldExplorePropagationImpactZone = pe.notificationBehavior match {
        case NotificationOnPropagateNoNotificationReceived | NotificationOnNotifyAndPropagate | NotificationOnPropagateReceivesNotification => true
        case NotificationOnNotifyNoPropagate | BulkElementNotificationBehavior | NoPropagationNotificationReceivedNoNotificationEmitted => false
        case SCCNotificationBehavior => ???
      }

      //the node has not been marked yet while exploring this layer
      require(pe.threadID > biggestTreadIdUsedSoFar, "error: node was already marked while exploring the same layer?!")
      require(currentPEIDInCurrentLayer == biggestTreadIdUsedSoFar - currentThreadIDForNodeMarking)

      pe.threadID = currentThreadIDForNodeMarking

      if(shouldExplorePropagationImpactZone) {
        arrayOfPEAndInterferingThreadIDs(currentPEIDInCurrentLayer) = (pe, markImpactZoneOfNotificationAndReportConflictingThreadIDMarkings(pe,
          currentThreadIDForNodeMarking,
          biggestTreadIdUsedSoFar))
      }else{
        arrayOfPEAndInterferingThreadIDs(currentPEIDInCurrentLayer) = (pe, null)
      }
      currentPEIDInCurrentLayer += 1
    }
    (arrayOfPEAndInterferingThreadIDs,currentPEIDInCurrentLayer)
  }


  def markImpactZoneOfNotificationAndReportConflictingThreadIDMarkings(pe:PropagationElement,
                                                                       currentThreadIDForNodeMarking:Int,
                                                                       maximalThreadIdMarkingOfCurrentLayer:Int,
                                                                       acc0:QList[Int] = null): QList[Int] = {

    def markCurrentPE(acc:QList[Int]):(QList[Int],Boolean) = {
      if (pe.threadID > maximalThreadIdMarkingOfCurrentLayer) {
        //the node has not been marked yet while exploring this layer
        //we mark it, nothing else to do
        pe.threadID = currentThreadIDForNodeMarking
        (acc0,false)
      } else {
        //this node was already marked as interfering with something in this layer, so it is recorded as interfering :(
        (QList(pe.threadID, acc0),true)
      }
    }

    def markStaticallyListenings(accIn:QList[Int]): QList[Int] ={
      var myAcc = accIn
      for (otherPE: PropagationElement <- pe.staticallyListeningElements) {
        myAcc = markImpactZoneOfNotificationAndReportConflictingThreadIDMarkings(
          otherPE,
          currentThreadIDForNodeMarking,
          maximalThreadIdMarkingOfCurrentLayer,
          myAcc)
      }
      myAcc
    }

    pe.notificationBehavior match {
      case NoPropagationNotificationReceivedNoNotificationEmitted | NotificationOnPropagateNoNotificationReceived =>
        acc0

      case BulkElement | NotificationOnNotifyNoPropagate | NotificationOnNotifyAndPropagate =>
        val (acc1,alreadyMarked) = markCurrentPE(acc0)
        if(alreadyMarked) acc1
        else markStaticallyListenings(acc1)

      case NotificationOnPropagateReceivesNotification =>
        val (acc1,alreadyMarked) = markCurrentPE(acc0)
        acc1
    }
  }
}


class MultiThreadRunner(nbSystemThread:Int,layerToNbThreads:Array[Int]) extends Runner(){
  val nbLayer = layerToNbThreads.length


  private[this] val layerToThreadToPEs: Array[Array[QList[PropagationElement]]] = layerToNbThreads.map(nbThread => Array.fill[QList[PropagationElement]](nbThread)(null))
  private[this] val layerToThreadWithSomething:Array[QList[Int]] = Array.fill[QList[Int]](nbLayer)(null)

  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayer)


  override def enqueue(pes: QList[PropagationElement]) {
    nonEmptyLayers.synchronized {
      var remainingPes = pes
      while(remainingPes != null){
        val pe = remainingPes.head
        remainingPes = remainingPes.tail

        val threadID = pe.threadID
        val layer = pe.layer

        val finalArray = layerToThreadToPEs(layer)
        val previousPEList = finalArray(threadID)
        finalArray(threadID) = QList(pe, previousPEList)

        if(previousPEList == null){
          val previousThreadsOfLayer = layerToThreadWithSomething(layer)
          layerToThreadWithSomething(layer) = QList(threadID,previousThreadsOfLayer)
          if(previousThreadsOfLayer == null) nonEmptyLayers.insert(layer)
        }
      }
    }
  }

  private[this] val threadPool = Executors.newFixedThreadPool(nbSystemThread)
  private[this] val threadIds = 0 until nbSystemThread

  override def performPropagation(upTo:PropagationElement) {

    upTo.schedulingHandler.enqueueForRun()

    while(nonEmptyLayers.nonEmpty) {
      //we fetch the stuff to propagate at this layer in a non-parallelized way
      //because propagation itself might insert more stuff to propagate
      val currentLayer = nonEmptyLayers.popFirst()

      val threadIDsToPEs = layerToThreadToPEs(currentLayer)
      var threadIDWithSomething = layerToThreadWithSomething(currentLayer)

      var toPropagateQLists:QList[QList[PropagationElement]] = null
      layerToThreadWithSomething(currentLayer) = null

      while(threadIDWithSomething != null){
        val currentThreadID = threadIDWithSomething.head
        threadIDWithSomething = threadIDWithSomething.tail
        require(threadIDsToPEs(currentThreadID) != null)
        toPropagateQLists = QList(threadIDsToPEs(currentThreadID), toPropagateQLists)
        threadIDsToPEs(currentThreadID) = null
      }

      var futuresForSynchro:QList[Future[_]] = null

      //multi-treading starts here
      //Start the propagation for each thread
      while(toPropagateQLists!=null){
        val pEsToPropagate:QList[PropagationElement] = toPropagateQLists.head
        toPropagateQLists = toPropagateQLists.tail

        val future:Future[_] = threadPool.submit(new Runnable {
          var myPEs = pEsToPropagate
          override def run(): Unit = {
            while (myPEs != null) {
              myPEs.head.performPropagation()
              myPEs = myPEs.tail
            }
          }
        })
        futuresForSynchro = QList(future,futuresForSynchro)
      }

      //wait for each task to complete
      while(futuresForSynchro!=null){
        futuresForSynchro.head.get()
        futuresForSynchro = futuresForSynchro.tail
      }
      //multi-treading ends here
    }
  }
}

