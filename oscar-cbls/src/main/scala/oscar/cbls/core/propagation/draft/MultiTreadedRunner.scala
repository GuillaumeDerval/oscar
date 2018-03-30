package oscar.cbls.core.propagation.draft

import java.util.concurrent.{Executors, Future}

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.propagation.draft.PropagationImpactCharacteristics._

class MultiTreadingPartitioningAlgo(layerToPropagationElements: Array[QList[PropagationElement]],
                                    layerToNbPropagationElements: Array[Int]){
  private val nbLayer:Int = layerToNbPropagationElements.length
  /**
    * partitions the graph into treads, allocate threadsIDs to PEs.
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
  private def allocateThreadIDForLayer(arrayOfMarkedNodesAndInterferingThreadIDs:Array[(PropagationElement,QList[Int])],offsetForNodeMarking:Int): Int = {
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

  private def markImpactZoneOfLayer(layer:Int,biggestTreadIdUsedSoFar:Int):(Array[(PropagationElement,QList[Int])],Int) = {

    var currentPEIDInCurrentLayer = 0
    val nbPEInCurrentLayer = layerToNbPropagationElements(layer)
    val arrayOfPEAndInterferingThreadIDs = Array.fill[(PropagationElement,QList[Int])](nbPEInCurrentLayer)(null)
    var currentThreadIDForNodeMarking = biggestTreadIdUsedSoFar

    for (pe <- layerToPropagationElements(layer)) {
      //We want to know about the nodes that will perform propagation, and notify on propagation to mark their notification

      currentThreadIDForNodeMarking = currentThreadIDForNodeMarking - 1

      //the node has not been marked yet while exploring this layer
      require(pe.threadID > biggestTreadIdUsedSoFar, "error: node was already marked while exploring the same layer?!")
      require(currentPEIDInCurrentLayer == biggestTreadIdUsedSoFar - currentThreadIDForNodeMarking)


      pe.threadID = currentThreadIDForNodeMarking
      arrayOfPEAndInterferingThreadIDs(currentPEIDInCurrentLayer) = (pe, markImpactZoneOfPropagationAndReportConflictingThreadIDMarkings(pe,
        currentThreadIDForNodeMarking,
        biggestTreadIdUsedSoFar))

      currentPEIDInCurrentLayer += 1
    }
    (arrayOfPEAndInterferingThreadIDs,currentPEIDInCurrentLayer)
  }

  private def markPEAndReportConflict(pe:PropagationElement,
                              currentThreadIDForNodeMarking:Int,
                              maximalThreadIdMarkingOfCurrentLayer:Int,
                              acc:QList[Int]):(QList[Int],Boolean) = {
    if (pe.threadID > maximalThreadIdMarkingOfCurrentLayer) {
      //the node has not been marked yet while exploring this layer
      //we mark it, nothing else to do
      pe.threadID = currentThreadIDForNodeMarking
      (acc,false)
    } else {
      require(pe.threadID != currentThreadIDForNodeMarking)
      //this node was already marked as interfering with something in this layer, so it is recorded as interfering :(
      (QList(pe.threadID, acc),true)
    }
  }

  /**
    * marks impact zone when propagatedPE is propagated
    * @param propagatedPE
    * @param currentThreadIDForNodeMarking
    * @param maximalThreadIdMarkingOfCurrentLayer
    * @param acc0
    * @return
    */
  private def markImpactZoneOfPropagationAndReportConflictingThreadIDMarkings(propagatedPE:PropagationElement,
                                                                      currentThreadIDForNodeMarking:Int,
                                                                      maximalThreadIdMarkingOfCurrentLayer:Int,
                                                                      acc0:QList[Int] = null): QList[Int] = {

    require(propagatedPE.scc == null)
    //pe is not in a SCC
    val (acc1, alreadyMarked) = markPEAndReportConflict(
      propagatedPE,
      currentThreadIDForNodeMarking,
      maximalThreadIdMarkingOfCurrentLayer,
      acc0)

    if (alreadyMarked) acc1
    else propagatedPE.notificationBehavior match {
      case NoPropagationNotificationReceivedNoNotificationEmitted =>
        acc1 //no impact zone
      case NotificationOnPropagateNoNotificationReceived
           | NotificationOnNotifyAndPropagate
           | NotificationOnPropagateReceivesNotification=>
        markImpactZoneOfNotifyingAndReportConflictingThreadIDMarkings(
          propagatedPE,
          currentThreadIDForNodeMarking,
          maximalThreadIdMarkingOfCurrentLayer,
          acc1)
      case BulkElementNotificationBehavior =>
        //this is a bulk element, will not propagate, actually
        acc1 //no impact zone
      case SCCNotificationBehavior =>
        //This is a SCC; upon propagation, it will perform a propagation of all its included PE
        require(propagatedPE.scc == null,"SCC cannot be nested into one another")
        var toReturn = acc1
        val theSCC = propagatedPE.asInstanceOf[StronglyConnectedComponent]
        for(peInSCC <- theSCC.propagationElements){
          toReturn = markImpactZoneOfPropagationAndReportConflictingThreadIDMarkings(
            peInSCC,
            currentThreadIDForNodeMarking,
            maximalThreadIdMarkingOfCurrentLayer,
            toReturn)
        }
        toReturn
      case NotificationOnNotifyNoPropagate =>
        acc1
    }
  }

  private def markImpactZoneOfNotifyingAndReportConflictingThreadIDMarkings(notifyingPE:PropagationElement,
                                                                    currentThreadIDForNodeMarking:Int,
                                                                    maximalThreadIdMarkingOfCurrentLayer:Int,
                                                                    acc0:QList[Int] = null): QList[Int] = {
    val (acc1, alreadyMarked) = markPEAndReportConflict(
      notifyingPE,
      currentThreadIDForNodeMarking,
      maximalThreadIdMarkingOfCurrentLayer,
      acc0)

    if (alreadyMarked) acc1
    else notifyingPE.notificationBehavior match {
      case NoPropagationNotificationReceivedNoNotificationEmitted
           | BulkElementNotificationBehavior
           | SCCNotificationBehavior =>
        throw new Error("no notifying PE explored for impact zone on notification")
      case NotificationOnNotifyNoPropagate
           | NotificationOnNotifyAndPropagate
           | NotificationOnPropagateReceivesNotification
           | NotificationOnPropagateNoNotificationReceived=>
        //this PE performs notification, follow its impact zone
        var toReturn = acc1
        for(notifiedPE <- notifyingPE.staticallyListeningElements){
          toReturn = markImpactZoneOfNotifiedAndReportConflictingThreadIDMarkings(notifiedPE,
            currentThreadIDForNodeMarking,
            maximalThreadIdMarkingOfCurrentLayer,
            toReturn)
        }
        toReturn
    }
  }

  private def markEnclosingSCCIfSomeAndReportConflict(pe:PropagationElement,
                                              currentThreadIDForNodeMarking:Int,
                                              maximalThreadIdMarkingOfCurrentLayer:Int,
                                              acc:QList[Int]):(QList[Int],Boolean) = {
    if(pe.scc == null) (null,false)
    else markPEAndReportConflict(pe.scc,
      currentThreadIDForNodeMarking,
      maximalThreadIdMarkingOfCurrentLayer,
      acc)
  }

  private def markImpactZoneOfNotifiedAndReportConflictingThreadIDMarkings(staticallyListeningPE:PropagationElement,
                                                                   currentThreadIDForNodeMarking:Int,
                                                                   maximalThreadIdMarkingOfCurrentLayer:Int,
                                                                   acc0:QList[Int] = null): QList[Int] = {
    //notifiedPE is possibly notified (unless it receives no notification, of course)
    //a PE in a SCC will notify its SCC
    staticallyListeningPE.notificationBehavior match {
      case NoPropagationNotificationReceivedNoNotificationEmitted | NotificationOnPropagateReceivesNotification=>
        val (acc1,sccMarked) = markEnclosingSCCIfSomeAndReportConflict(staticallyListeningPE,
          currentThreadIDForNodeMarking,
          maximalThreadIdMarkingOfCurrentLayer,
          acc0)
        if(sccMarked) acc1
        else markPEAndReportConflict(
          staticallyListeningPE,
          currentThreadIDForNodeMarking,
          maximalThreadIdMarkingOfCurrentLayer,
          acc0)._1
      case BulkElementNotificationBehavior | NotificationOnNotifyNoPropagate | NotificationOnNotifyAndPropagate=>
        val (acc1,sccMarked) = markEnclosingSCCIfSomeAndReportConflict(staticallyListeningPE,
          currentThreadIDForNodeMarking,
          maximalThreadIdMarkingOfCurrentLayer,
          acc0)
        if(sccMarked) acc1
        else {
          //we first mark this one,
          // and if not marked, forward the notification to the listening PEs
          val (acc1, alreadyMarked) = markPEAndReportConflict(
            staticallyListeningPE,
            currentThreadIDForNodeMarking,
            maximalThreadIdMarkingOfCurrentLayer,
            acc0)
          if (alreadyMarked) acc1
          else {
            var toReturn = acc1
            for (staticallyListeningPE2 <- staticallyListeningPE.staticallyListeningElements) {
              toReturn = markImpactZoneOfNotifiedAndReportConflictingThreadIDMarkings(
                staticallyListeningPE2,
                currentThreadIDForNodeMarking,
                maximalThreadIdMarkingOfCurrentLayer,
                toReturn)
            }
            toReturn
          }
        }
      case SCCNotificationBehavior =>
        throw new Error("SCC is not statically listening to anything")
      case NotificationOnPropagateNoNotificationReceived =>
        //it will not receive any notification, so we actually can ignore it
        acc0
    }
  }
}

class MultiThreadRunner(nbSystemThread:Int,layerToNbThreads:Array[Int]) extends Runner(){
  val nbLayer = layerToNbThreads.length

  private[this] val layerToThreadToPEs: Array[Array[QList[PropagationElement]]] = layerToNbThreads.map(nbThread => Array.fill[QList[PropagationElement]](nbThread)(null))
  private[this] val layerToThreadWithSomething:Array[QList[Int]] = Array.fill[QList[Int]](nbLayer)(null)
  private[this] val nonEmptyLayers: BinomialHeap[Int] = new BinomialHeap[Int]((item: Int) => item, nbLayer)

  @inline
  override protected def enqueuePENS(pe:PropagationElement) {
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

  private[this] val threadPool = Executors.newFixedThreadPool(nbSystemThread)

  override def runSH(upTo:SchedulingHandler) {
    require(upTo.runner == this)
    upTo.enqueueForRun()

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

      require(toPropagateQLists != null, "error: layer to propagate with no PE in it")
      if(toPropagateQLists.tail == null){
        //only one thread, so we do not spawn propagation, just do it with current thread
        var myPEs = toPropagateQLists.head
        while (myPEs != null) {
          myPEs.head.propagate()
          myPEs = myPEs.tail
        }
        toPropagateQLists = null
      }else{
        //more than one thread needed here, wso we spawn the work on the available worker threads

        var futuresForSynchro:QList[Future[_]] = null

        //multi-treading starts here
        //Start the propagation for each thread
        while(toPropagateQLists!=null){
          futuresForSynchro = QList(
            threadPool.submit(new PESRunner(toPropagateQLists.head)),
            futuresForSynchro)
          toPropagateQLists = toPropagateQLists.tail
        }

        //wait for each task to complete
        while(futuresForSynchro!=null){
          futuresForSynchro.head.get() //this blocks until the associated computation completes
          futuresForSynchro = futuresForSynchro.tail
        }
        //multi-treading ends here
      }
    }

    upTo.notifyEndRun()
  }
}

class PESRunner(pEsToPropagate:QList[PropagationElement]) extends Runnable{
  override def run(): Unit ={
    var myPEs = pEsToPropagate
    while (myPEs != null) {
      myPEs.head.propagate()
      myPEs = myPEs.tail
    }
  }
}
