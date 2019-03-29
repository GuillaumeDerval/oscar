package oscar.cbls.test.unit

import org.scalatest.{AppendedClues, FunSuite, Matchers}
import org.scalatest.prop.Checkers
import oscar.cbls._
import oscar.cbls.algo.clique.Clique
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, Edge, FloydWarshall, RandomGraphGenerator}
import oscar.cbls.algo.quick.QList
import oscar.cbls.lib.invariant.graph.{DistanceInConditionalGraph, VoronoiZones}
import oscar.cbls.test.invariants.bench._


class VoronoiZonesTestSuite extends FunSuite with Matchers with AppendedClues {

  val verbose = 0

  test("Voronoi Zones in conditional graph"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val nbCentroids = 10L

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"openConditions")
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids,"centroids")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes,
      mapSide = 1000)

    VoronoiZones(graph,
      graphDiameterOverApprox = Long.MaxValue-1,
      openConditions,
      centroids:SetValue,
      trackedNodes = nbCentroids until nbNodes,
      openConditions.model,
      defaultDistanceForUnreachableNodes = Long.MaxValue)

    bench.run()
  }

  test("Voronoi Zones in conditional graph with non transit nodes"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val nbCentroids = 10L

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges, name="openConditions")
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    VoronoiZones(graph,
      graphDiameterOverApprox = Long.MaxValue-1,
      openConditions,
      centroids:SetValue,
      trackedNodes = nbCentroids until nbNodes,
      openConditions.model,
      defaultDistanceForUnreachableNodes = Long.MaxValue)

    bench.run()
  }

  test("Voronoi.SpanningTree contains all the given nodes"){

    for (i <- 0 until 100) {
      println("***********")
      val bench = new InvBench(verbose, List(PlusOne()))

      val nbNodes = 15
      val nbConditionalEdges = 0
      val nbNonConditionalEdges = 80
      val nbCentroids = 3L

      val openConditions: CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 to nbConditionalEdges, name = "openConditions")
      val centroids: CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes,
        mapSide = 1000)

      val voronoi = VoronoiZones(graph,
        graphDiameterOverApprox = Long.MaxValue - 1,
        openConditions,
        centroids: SetValue,
        trackedNodes = nbCentroids until nbNodes,
        openConditions.model,
        defaultDistanceForUnreachableNodes = Long.MaxValue)

      val tree = voronoi.spanningTree(QList.buildFromIterable(graph.nodes))
      val edgeList = tree.toList

      println(centroids.value.mkString(","))
      println(exportGraphToNetworkxInstructions(graph, openConditions.value.toList, edgeList))
      graph.nodes.forall(n => {
        val count = edgeList.count(e => e.nodeIDA == n.id || e.nodeIDB == n.id)
        val isCentroid = centroids.value.contains(n.id)

        println(s"${n.id} -> $count. IsCentroid -> $isCentroid")
        count > 0 || isCentroid
      }) should be(true)
    }
  }

  def exportGraphToNetworkxInstructions(graph :ConditionalGraphWithIntegerNodeCoordinates, openConditions :List[Long],spanningTree :List[Edge] = List()): String ={

    var toReturn = s"nbNodes = ${graph.nbNodes}\n"

    val nonConditionalEdges = graph.edges.filter(e => e.conditionID.isEmpty).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val openEdges =  graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 1)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val closeEdges = graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 0)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val nodesPositions = graph.coordinates.zipWithIndex.map({case (e,i) => s"$i : (${e._1},${e._2})"}).mkString(",")
    val spanningTreeString = spanningTree.map(e => s"(${e.nodeIDB},${e.nodeIDA})").mkString(",")

    toReturn = toReturn.concat(s"openEdges = [$openEdges]\n")
    toReturn = toReturn.concat(s"closedEdges = [$closeEdges]\n")
    toReturn = toReturn.concat(s"nonConditionalEdges = [$nonConditionalEdges]\n")
    toReturn = toReturn.concat(s"pos = {$nodesPositions}\n")
    toReturn = toReturn.concat(s"span = [$spanningTreeString]")

    toReturn
  }
}
