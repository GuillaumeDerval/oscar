package oscar.cbls.core.draft.propagation

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.tarjan.{TarjanNodeData, TarjanWithExternalStorage}

class SchedulingHandlerPartitioningAlgo(propagationStructure:PropagationStructure) {

  /**
    * identifies SCC in the propagation graph,
    * creates SCC SH
    * this might create additional PE
    * @return the PE not in SCC (including the newly created PE) and the SCC
    */
  def identifyAndInstantiateSCC(): (QList[PropagationElement], QList[StronglyConnectedComponent]) = {
    val storageForTarjan = propagationStructure.buildTabulatedNodeStorage[TarjanNodeData](_ => new TarjanNodeData)

    val stronglyConnectedComponents: List[QList[PropagationElement]] =
      TarjanWithExternalStorage.getStronglyConnexComponents[PropagationElement](
        propagationStructure.allPropagationElements,
        p => p.staticallyListeningElements,
        storageForTarjan(_))

    var acyclic = true
    var stronglyConnectedComponentsStructures: QList[StronglyConnectedComponent] = null
    var propagationElementsNotInSCC: QList[PropagationElement] = null

    for (pEOfSCC <- stronglyConnectedComponents) {
      if (pEOfSCC.tail == null) {
        //only one PE, so not in a SCC
        propagationElementsNotInSCC = QList(pEOfSCC.head, propagationElementsNotInSCC)
      } else {
        //a new SCC must be instantiated here
        acyclic = false
        val scc = new StronglyConnectedComponent(pEOfSCC, pEOfSCC.size, propagationStructure)
        stronglyConnectedComponentsStructures = QList(scc, stronglyConnectedComponentsStructures)

        //we also add the call back PE of the SCC
        propagationElementsNotInSCC = QList(scc.myCallBackPE, propagationElementsNotInSCC)
      }
    }

    (propagationElementsNotInSCC, stronglyConnectedComponentsStructures)
  }

  /**
    * this method creates scheduling handlers
    * for all propagation elements with dynamic dependencies
    * SCC must already have been allocated
    * this might create additional PE, added to the PS
    */
  def instantiateVariableSchedulingHandlersForPENotInSCC(): Unit = {
    for (pe <- propagationStructure.allPropagationElements if pe.scc.isDefined) {
      pe match {
        case pev: PropagationElement with VaryingDependencies =>
          val dsh = new SchedulingHandlerForPEWithVaryingDependencies(pev, propagationStructure)
      }
    }
  }


  /**
    * partitions teh graph into SH, taking into account the already allocated SH
    * (SCC and SH for PE with varying dependencies)
    * this also performs global registration of all SH to their listening SH
    */
  def partitionGraphIntoSchedulingHandlers(layerToPropagationElements:Array[QList[PropagationElement]]) {
    var currentLayerID = layerToPropagationElements.length
    while (currentLayerID > 0) {
      currentLayerID = currentLayerID - 1

      //from outputs to inputs decorate layer by layer
      //the SH of a node is the SH of its successors
      //except if the node already has a sh or if its successors have different sh's or if the SH of its successor is a VSH or if the node has no successor at all
      //in this case, a new SH is instantiated

      for (pe <- layerToPropagationElements(currentLayerID)) {

        if (pe.schedulingHandler != null){
          //there is already a SH at this node, we need to register to the listening SH

          val currentSH = pe.schedulingHandler

          var allListeningSH: QList[SchedulingHandler] = null

          def addListeningSH(sh: SchedulingHandler) {
            if (!sh.isRunning && currentSH != sh) {
              sh.isRunning = true
              allListeningSH = QList(sh, allListeningSH)
              currentSH.addListeningSchedulingHandler(sh)
            }
          }

          var staticallyListeningElements = pe.staticallyListeningElements
          while (staticallyListeningElements != null) {
            addListeningSH(staticallyListeningElements.head.schedulingHandler)
            staticallyListeningElements = staticallyListeningElements.tail
          }

          while (allListeningSH != null) {
            allListeningSH.head.isRunning = false
            allListeningSH = allListeningSH.tail
          }

        }else{
          //we need to set a scheduling handler to this node

          var staticallyListeningElements = pe.staticallyListeningElements

          if (staticallyListeningElements == null) {
            //it has no successor, so it gets a new scheduling handler and job is done.
            //no SH listen to this SH either

            pe.schedulingHandler = new SimpleSchedulingHandler(propagationStructure)

          } else {
            //it has some successor, so we need to check if they all have the same scheduling handler
            val referenceListeningSchedulingHandler = null
            var newSHNeededSoFar = false
            while (staticallyListeningElements != null && !newSHNeededSoFar) {

              staticallyListeningElements.head.schedulingHandler match {
                case scc: StronglyConnectedComponent =>
                  newSHNeededSoFar = true //we add a new SH
                case simple: SimpleSchedulingHandler =>
                  if (referenceListeningSchedulingHandler == null) {
                    referenceListeningSchedulingHandler == simple
                    staticallyListeningElements = staticallyListeningElements.tail
                  } else if (referenceListeningSchedulingHandler != simple) {
                    //it already has a SH, and it is another one, so we need a new SH
                    newSHNeededSoFar = true //we add a new SH
                  } else {
                    staticallyListeningElements = staticallyListeningElements.tail
                  }
                case dyn: SchedulingHandlerForPEWithVaryingDependencies =>
                  newSHNeededSoFar = true //we add a new SH
              }
            }

            if (newSHNeededSoFar) {
              val newSchedulingHandler = new SimpleSchedulingHandler(propagationStructure)
              pe.schedulingHandler = newSchedulingHandler

              var allListeningSH: QList[SchedulingHandler] = null

              def addListeningSH(sh: SchedulingHandler) {
                if (!sh.isRunning) {
                  sh.isRunning = true
                  allListeningSH = QList(sh, allListeningSH)
                  newSchedulingHandler.addListeningSchedulingHandler(sh)
                }
              }

              addListeningSH(referenceListeningSchedulingHandler)

              while (staticallyListeningElements != null) {
                addListeningSH(staticallyListeningElements.head.schedulingHandler)
                staticallyListeningElements = staticallyListeningElements.tail
              }

              while (allListeningSH != null) {
                allListeningSH.head.isRunning = false
                allListeningSH = allListeningSH.tail
              }

            } else {
              pe.schedulingHandler = referenceListeningSchedulingHandler
              //There is no ned to register to any listening SH here because we do not create a new SH
            }
          }
        }
      }
    }
  }

  def generatePropagationGraphDot(allSchedulingHandlers:QList[SchedulingHandler]):String = {
    val acc:StringBuilder = new StringBuilder()

    val sHArray:Array[SchedulingHandler] = allSchedulingHandlers.toArray
    //Each SH is a subgraph

    //PE are identified by their ID

    //SH are identified by
    //their order of appearance in the list of all scheduling handlers in the structure
    //this is slow, but we do not care at all

    def getSHID(sh:SchedulingHandler):Int = {
      sHArray.indexOf(sh)
    }

    acc + "digraph G {\n"

    for (shID <- sHArray.indices){
      val sh = sHArray(shID)
      acc + s"\tsubgraph sh_$shID {\n"

      val pEs:Iterable[PropagationElement] = sh match{
        case simple:SimpleSchedulingHandler =>
          acc + s"\t\tlabel = \"sh_$shID (simple)\";\n"
          propagationStructure.allPropagationElements.filter(_.schedulingHandler == simple)
        case scc:StronglyConnectedComponent =>
          acc + s"\t\tlabel = \"sh_$shID (SCC)\";\n"
          scc.propagationElements
        case dyn:SchedulingHandlerForPEWithVaryingDependencies =>
          acc + s"\t\tlabel = \"sh_$shID (Dyn)\";\n"
          QList(dyn.p)
      }

      for(pe <- pEs){
        acc+ "\t\tpe_" + pe.uniqueID + " [label=\"" + pe.toString + "\" style=filled color=salmon2]; \n"
      }
      acc + s"\t}\n"
    }

    for(peFrom <- propagationStructure.allPropagationElements){
      for (peTo <- peFrom.staticallyListeningElements){
        acc + "\tpe_" + peFrom.uniqueID + " -> pe_" + peTo.uniqueID + ";\n"
      }
    }

    acc + "}\n\n"

    acc.mkString
  }
}

