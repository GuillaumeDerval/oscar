package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.{HashSet, List}
import scala.collection.mutable

/**
  * Created by fg on 12/09/17.
  */
class Chains(vrp: VRP, chains: List[List[Int]]) extends VRPExtension(vrp){

  val (chainOfNode, nextNodeInChain, prevNodeInChain, nextNodesInChain, prevNodesInChain) = {

    val chainOfNode: Array[List[Int]] = Array.fill(vrp.n)(List.empty)
    val nextNodeInChain: Array[Option[Int]] = Array.fill(vrp.n)(None)
    val prevNodeInChain: Array[Option[Int]] = Array.fill(vrp.n)(None)
    val nextNodesInChain: Array[List[Int]] = Array.fill(vrp.n)(List.empty)
    val prevNodesInChain: Array[List[Int]] = Array.fill(vrp.n)(List.empty)

    def proceedChain(toProceed: List[Int], previousNodes:List[Int],chain:List[Int]): Unit ={
      toProceed match {
        case Nil =>
        case head :: tail =>
          chainOfNode(head) = chain
          if(previousNodes.nonEmpty)nextNodeInChain(previousNodes.last) = Some(head)
          prevNodeInChain(head) = previousNodes.lastOption
          nextNodesInChain(head) = tail
          prevNodesInChain(head) = previousNodes

          proceedChain(tail,previousNodes :+ head,chain)
      }
    }

    for(chain <- chains) proceedChain(chain,List.empty,chain)
    (chainOfNode,nextNodeInChain,prevNodeInChain,nextNodesInChain,prevNodesInChain)
  }

  val heads = chains.map(_.head)

  def firstNodeInChainOfNode(node: Int): Int = chainOfNode(node).head
  def lastNodeInChainOfNode(node: Int): Int = chainOfNode(node).last

  def isHead(node: Int): Boolean = chainOfNode(node).head == node
  private def isLast(node: Int): Boolean = chainOfNode(node).last == node


  def preComputeRelevantNeighborsForFirstNode(firstNode: Int): Iterable[Int] ={
    require(isHead(firstNode), "The referenced node has to be the first node of a chain.")
    vrp.preComputedRelevantNeighborsOfNodes(firstNode)
  }

  def computeRelevantNeighborsForLastNode(lastNode: Int): Iterable[Int] = {
    require(isLast(lastNode), "The referenced node has to be the last node of a chain.")
    val potentialRelevantNeighborsOfLastNode: List[Int] =
      vrp.preComputedRelevantNeighborsOfNodes(lastNode)

    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(chainOfNode(lastNode).head)
    var relevantNeighborsOfLastNode: List[Int] = List.empty
    while (nextOfHeadExplorer match{
      case Some(x) if x.value < vrp.v =>
        false
      case Some(x) if potentialRelevantNeighborsOfLastNode.contains(x.value) =>
        relevantNeighborsOfLastNode = x.value :: relevantNeighborsOfLastNode
        nextOfHeadExplorer = x.next
        true
      case _ => false
    }){}
    relevantNeighborsOfLastNode
  }

  def computeRelevantNeighborsForInternalNodes()(node: Int): Iterable[Int] ={
    val firstNode = prevNodeInChain(node)
    val lastNode = chainOfNode(node).last
    require(firstNode.isDefined && lastNode != chainOfNode(node).last,
      "This method is designed to insert or move nodes between the start and the end of a chain")
    require(vrp.isRouted(firstNode.get) && vrp.isRouted(lastNode), "The beginning node and last node of the chain must be routed")
    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(firstNode.get).head.next
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

  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: List[Int]): List[Int] = {
    potentialRelevantNeighbors
  }

  override def postFilter(node: Int) = (neighbor: Int) => true
}

class ChainsExtensionBuilder(vrp: VRP){

  private val chains: mutable.HashSet[List[Int]] = mutable.HashSet.empty

  /**
    * Add one chain of nodes to the chains list
    * @param chain The chain to add
    */
  def addChain(chain: List[Int]): Unit ={
    chains.add(chain)
  }

  /**
    * Add a list of chains of nodes to the chains list
    * @param chains The chains to add
    */
  def addChains(chains: List[List[Int]]){
    chains foreach this.chains.add
  }

  /**
    * Build and return a Chains object
    * @return
    */
  def build(): Chains ={
    new Chains(vrp, chains.toList)
  }

}
