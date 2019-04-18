package oscar.cbls.algo.graph

/**
  * this algo simplifies graphes by
  *  removing non connected components,
  *  collapsing linear strings into single edge
  *
  *  it returns a ContractedConditionalGraph.
  *  It is a conditional graph, which has a correspondence between the new nodes and the old nodes
  */
object GraphContractor {

  def apply(g:ConditionalGraph, nodesToKeep:Iterable[Node]):ContractedConditionalGraph = ???

}


class ContractedConditionalGraph(originalGraph:ConditionalGraph,
                                 nodes:Array[Node],
                                 edges:Array[Edge],
                                 nodeToOriginalNodes:Array[Node],
                                 originalNodeToNode:Array[Node],
                                 edgeToPathInOriginalGraph:Array[List[Edge]])
  extends ConditionalGraph(nodes,edges,originalGraph.nbConditions) {

  def extendPath(l:List[Edge]):List[Edge] = ???


}

