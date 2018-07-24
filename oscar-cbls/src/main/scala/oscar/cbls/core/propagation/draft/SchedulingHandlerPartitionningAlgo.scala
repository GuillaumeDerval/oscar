package oscar.cbls.core.propagation.draft

import scala.collection.immutable.SortedSet

class SchedulingHandlerPartitioningAlgo(p:PropagationStructure){

  def instantiateVariableSchedulingHandlers(): Unit ={

  }

  //TODO how about dynamic dependencies?
  //TODO how about SCC?
  //TODO how about statically listening that are in a SCC?

  //graph must already be sorted by layers
  def partitionIntoSchedulingHandlers(){
    var currentLayerID = p.layerToClusteredPropagationElements.length
    while(currentLayerID >0){
      currentLayerID = currentLayerID -1

      //from outputs to inputs decorate layer by layer
      //the SH of a node is the SH of its successors
      //except if the node already has a sh or if its successors have different sh's

      for(pe <- p.layerToClusteredPropagationElements(currentLayerID)){

        if (pe.schedulingHandler == p) {
          //it needs to be decorated

          var staticallyListeningElements = pe.staticallyListeningElements

          if (staticallyListeningElements == null) {
            //it has no succesor, so it gets a new scheduling handler and job is done.

            val newSchedulingHandler = new SimpleSchedulingHandler()
            p.registerSchedulingHandler(newSchedulingHandler)
            pe.schedulingHandler = newSchedulingHandler

          } else {
            val referenceListeningSchedulingHandler = staticallyListeningElements.head.schedulingHandler
            staticallyListeningElements = staticallyListeningElements.tail
            while (staticallyListeningElements != null) {
              if (staticallyListeningElements.head.schedulingHandler != referenceListeningSchedulingHandler) {
                //there are more than one listening scheduling handler, so we create a scheduling handler on pe
                val newSchedulingHandler = new SimpleSchedulingHandler()
                p.registerSchedulingHandler(newSchedulingHandler)
                pe.schedulingHandler = newSchedulingHandler

                newSchedulingHandler.addListeningSchedulingHandler(referenceListeningSchedulingHandler)
                newSchedulingHandler.addListeningSchedulingHandler(staticallyListeningElements.head.schedulingHandler)

                var knownIDs: SortedSet[Int] = SortedSet(referenceListeningSchedulingHandler.uniqueIDSH,
                  staticallyListeningElements.head.schedulingHandler.uniqueIDSH)

                while (staticallyListeningElements != null) {
                  val sh = staticallyListeningElements.head.schedulingHandler
                  staticallyListeningElements = staticallyListeningElements.tail

                  if (!(knownIDs contains sh.uniqueIDSH)) {
                    knownIDs = knownIDs + sh.uniqueIDSH
                    newSchedulingHandler.addListeningSchedulingHandler(sh)
                  }
                }
              } else {
                staticallyListeningElements = staticallyListeningElements.tail
              }
            }

          }
        }else{
          //it has already a scheduling handler, so we add all listening scheduling handler to this one
          var staticallyListeningElements = pe.staticallyListeningElements
          var knownIDs: SortedSet[Int] = SortedSet.empty

          while (staticallyListeningElements != null) {
            val sh = staticallyListeningElements.head.schedulingHandler
            staticallyListeningElements = staticallyListeningElements.tail

            if (!(knownIDs contains sh.uniqueIDSH)) {
              knownIDs = knownIDs + sh.uniqueIDSH
              pe.schedulingHandler.addListeningSchedulingHandler(sh)
            }
          }
        }
      }
    }
  }
}
