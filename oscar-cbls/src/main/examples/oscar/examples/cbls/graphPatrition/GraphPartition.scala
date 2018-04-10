package oscar.examples.cbls.graphPatrition

import oscar.cbls._
import oscar.cbls.lib.invariant.logic.DenseCount
import oscar.cbls.modeling._

import scala.util.Random

object GraphPartition extends CBLSModel with App {

  val nbNodes:Int = 1000
  val nbEdges:Int = nbNodes * nbNodes / 100

  require(nbNodes % 2 == 0, "nbnodes must be even")

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

  val Array(nbNodesInCluster0,nbNodesInCluster1) = DenseCount.makeDenseCount(nodeToPartition).counts

  val sameSizeConstraint = nbNodesInCluster0 === nbNodesInCluster1
  post(sameSizeConstraint, nbNodes) //we put some large weight on this constraint

  val nodeToViolation = c.violations(nodeToPartition)
  val mostViolatedNodes = argMax(nodeToViolation)
  val violatedNodes = filter(nodeToViolation)

  c.close()

  val obj = Objective(c.violation)

  close()

  //there is a lot of possible neighborhoods. some are more connected, some are faster.
  //TODO: propoze a combinator that can take such properties into account, and provide a faster global search
  val neighborhood =(
    bestSlopeFirst(
      List(
        profile(assignNeighborhood(nodeToPartition, "moveAll")),
        //profile(swapsNeighborhood(nodeToPartition, "swapAll")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => mostViolatedNodes.value, name = "swap1MostViol")),
        //profile(swapsNeighborhood(nodeToPartition, //this one is the most complete of swaps, but highly inefficient compared tpo the others,and I think that it does not bring in more connexity than others (althrough I am not so suer...)
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone1 = () => violatedNodes.value, name = "swap1Viol")),
        //profile(swapsNeighborhood(nodeToPartition,
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone1 = () => violatedNodes.value,
        //  searchZone2 = (_,_) => violatedNodes.value,
        //  name = "swap2Viol")),
        profile(swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = mostViolatedNodes,
          searchZone2 = (_,_) => violatedNodes.value,
          name = "swap1Viol1Most")),
        profile(swapsNeighborhood(nodeToPartition,
          searchZone1 = mostViolatedNodes,
          searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
          hotRestart = false,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1MostVAdj")),
        profile(swapsNeighborhood(nodeToPartition,
          searchZone1 = violatedNodes,
          searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
          hotRestart = false,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1ViolAdj")),
        //profile(swapsNeighborhood(nodeToPartition,
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
        //  name = "swapAdjacent"))
      ),refresh = nbNodes/10)
      onExhaustRestartAfter(randomizeNeighborhood(nodeToPartition, () => nbNodes/100, name = "randomize" + nbNodes/100), 2, obj)
    exhaust profile(swapsNeighborhood(nodeToPartition, //this one is the most complete of swaps, but highly inefficient compared tpo the others,and I think that it does not bring in more connexity than others (althrough I am not so suer...)
      symmetryCanBeBrokenOnIndices = false,
      searchZone1 = violatedNodes, name = "swap1Viol")))

  neighborhood.verbose = 1
  neighborhood.doAllMoves(_ >= nbNodes + nbEdges, obj)

  println(neighborhood.profilingStatistics)

  println("violation on sameSize(partitions): " + sameSizeConstraint.violation.value)
}

