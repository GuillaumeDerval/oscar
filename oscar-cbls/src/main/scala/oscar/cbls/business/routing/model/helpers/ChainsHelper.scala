package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.extensions.Chains

import scala.collection.immutable.{HashSet, List}

/**
  * Created by fg on 12/09/17.
  */
object ChainsHelper {

  def relevantNeighborsForLastNodeAfterHead(vrp: VRP, chainsExtension: Chains, potentialRelevantPredecessorOfLastNode: Option[HashSet[Int]] = None)(lastNode: Int): Iterable[Int] = {
    require(chainsExtension.isLast(lastNode), "The referenced node has to be the last node of a chain.")
    val potentialRelevantPredecessorOfLastNodeNow = potentialRelevantPredecessorOfLastNode.getOrElse(HashSet(vrp.routed.value.toList: _*))
    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(chainsExtension.chainOfNode(lastNode).head)
    var relevantNeighborsOfLastNode: List[Int] = List.empty
    while (nextOfHeadExplorer match{
      case Some(x) if x.value < vrp.v =>
        false
      case Some(x) if potentialRelevantPredecessorOfLastNodeNow.contains(x.value) =>
        relevantNeighborsOfLastNode = x.value :: relevantNeighborsOfLastNode
        nextOfHeadExplorer = x.next
        true
      case _ => false
    }){}
    relevantNeighborsOfLastNode
  }

  def computeRelevantNeighborsForInternalNodes(vrp: VRP, chainsExtension: Chains)(node: Int): Iterable[Int] ={
    val firstNode = chainsExtension.prevNodeInChain(node)
    val lastNode = chainsExtension.chainOfNode(node).last
    require(firstNode.isDefined && lastNode == chainsExtension.chainOfNode(node).last && lastNode != node,
      "This method is designed to insert or move nodes between the start and the end of a chain")
    require(vrp.isRouted(firstNode.get) && vrp.isRouted(lastNode),
      "The beginning node and last node of the chain must be routed")
    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(firstNode.get)
    var relevantNeighbors: List[Int] = List.empty
    while (nextOfHeadExplorer match{
      case Some(x) if x.value != lastNode =>
        relevantNeighbors = x.value :: relevantNeighbors
        nextOfHeadExplorer = x.next
        true
      case _ => false
    }){}
    relevantNeighbors
  }
}