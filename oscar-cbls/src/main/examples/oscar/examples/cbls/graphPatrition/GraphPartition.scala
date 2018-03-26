package oscar.examples.cbls.graphPatrition

import oscar.cbls._
import oscar.cbls.modeling._
import oscar.examples.cbls.graphPatrition.GraphPartition.swapsNeighborhood

import scala.util.Random

object GraphPartition extends CBLSModel with App {

  val nbNodes:Int = 1000
  val nbEdges:Int = nbNodes * nbNodes / 100

  def generateRandomEdges(nbNodes:Int,nbEdges:Int):(List[(Int,Int)],Array[List[Int]]) = {
    val adjacencyLists:Array[List[Int]] = Array.fill(nbNodes)(List.empty)
    val allEdges = List.tabulate(nbEdges)(_ => {
      val nodeA = Random.nextInt(nbNodes)
      val nodeB = Random.nextInt(nbNodes)
      adjacencyLists(nodeA) = nodeB :: adjacencyLists(nodeA)
      adjacencyLists(nodeB) = nodeA :: adjacencyLists(nodeB)
      (nodeA,nodeB)
    })
    (allEdges,adjacencyLists)
  }

  val (edges,adjacencyLists) = generateRandomEdges(nbNodes,nbEdges)

  val nodeToPartition = Array.tabulate(nbNodes)((nodeID:Int) => CBLSIntVar(if(Random.nextBoolean()) 1 else 0, 0 to 1, "partitionOfNode_" + nodeID))

  for((nodeA,nodeB) <- edges){
    post(nodeToPartition(nodeA) === nodeToPartition(nodeB))
  }

  val Array(nodeInCluster0,nodesInCluster1) = makeDenseCluster(nodeToPartition).clusters
  val nbNodesInCluster0 = cardinality(nodeInCluster0)
  val nbNodesInCluster1 = cardinality(nodesInCluster1)

  val sameSizeConstraint = nbNodesInCluster0 === nbNodesInCluster1
  post(sameSizeConstraint, nbNodes) //we put some large weight on this constraint

  val nodeToViolation = c.violations(nodeToPartition)
  val mostViolatedNodes = argMax(nodeToViolation)
  val violatedNodes = filter(nodeToViolation)

  c.close()

  val obj = Objective(c.violation)

  close()

  val neighborhood =(
    bestSlopeFirst(
      List(
        profile(assignNeighborhood(nodeToPartition, "moveAll")),
        //profile(swapsNeighborhood(nodeToPartition, "swapAll")), this one seems to be not larger than swap1Viol so I disabled it
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => mostViolatedNodes.value, name = "swap1MostViol")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => violatedNodes.value, name = "swap1Viol")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => violatedNodes.value,
          searchZone2 = (_,_) => violatedNodes.value,
          name = "swap2Viol")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => mostViolatedNodes.value,
          searchZone2 = (_,_) => violatedNodes.value,
          name = "swap1Viol1Most")),
        profile(swapsNeighborhood(nodeToPartition,
          searchZone1 = () => mostViolatedNodes.value,
          searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
          hotRestart = false,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1MostVAdj")),
        profile(swapsNeighborhood(nodeToPartition,
          searchZone1 = () => violatedNodes.value,
          searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
          hotRestart = false,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1ViolAdj")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
          name = "swapAdjacent"))
      ),refresh = nbNodes/10)
      onExhaustRestartAfter(randomizeNeighborhood(nodeToPartition, () => nbNodes/10), 2, obj))

  neighborhood.verbose = 1
  neighborhood.doAllMoves(_ >= nbNodes + nbEdges, obj)

  println(neighborhood.profilingStatistics)

  println("violation on sameSize(partitions): " + sameSizeConstraint.violation.value)
}

