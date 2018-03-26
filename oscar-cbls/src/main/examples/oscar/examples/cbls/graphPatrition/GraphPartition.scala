package oscar.examples.cbls.graphPatrition

import oscar.cbls._
import oscar.cbls.modeling._

import scala.util.Random

object GraphPartition extends CBLSModel with App {

  val nbNodes:Int = 1000
  val nbEdges:Int = nbNodes * nbNodes / 10

  def generateRandomEdges(nbNodes:Int,nbEdges:Int):List[(Int,Int)] = {
    List.tabulate(nbEdges)(_ => (Random.nextInt(nbNodes),Random.nextInt(nbNodes)))
  }

  val edges = generateRandomEdges(nbNodes,nbEdges)

  val nodeToPartition = Array.tabulate(nbNodes)((nodeID:Int) => CBLSIntVar(if(Random.nextBoolean()) 1 else 0, 0 to 1, "partitionOfNode_" + nodeID))

  for((nodeA,nodeB) <- edges){
    post(nodeToPartition(nodeA) === nodeToPartition(nodeB))
  }

  val Array(nodeInCluster0:CBLSIntVar,nodesInCluster1:CBLSIntVar) = makeDenseCluster(nodeToPartition).clusters
  val nbNodesInCluster0 = cardinality(nodeInCluster0)
  val nbNodesInCluster1 = cardinality(nodesInCluster1)

  post(nbNodesInCluster0 === nbNodesInCluster1, nbNodes) //we put some large weight on this constraint

  close()

  val neighborhood =(
    bestSlopeFirst(
      List(
        assignNeighborhood(nodeToPartition, "moveNodeToOtherPartition"),
        swapsNeighborhood(nodeToPartition, "swapNodes")

      ),refresh = nbNodes/10)
      onExhaustRestartAfter(randomizeNeighborhood(nodeToPartition, () => nbNodes/10), 2, c.violation))


  neighborhood.doAllMoves(_ >= nbNodes + nbEdges, c.violation)


}

