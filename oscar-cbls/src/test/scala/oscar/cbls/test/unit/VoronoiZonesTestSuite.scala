package oscar.cbls.test.unit

import org.scalatest.{AppendedClues, FunSuite, Matchers}
import org.scalatest.prop.Checkers
import oscar.cbls._
import oscar.cbls.algo.graph.{FloydWarshall, RandomGraphGenerator}
import oscar.cbls.lib.invariant.graph.{DistanceInConditionalGraph, VoronoiZones}
import oscar.cbls.test.invariants.bench._


class VoronoiZonesTestSuite extends FunSuite with Matchers with AppendedClues {

  val verbose = 0

  test("Voronoi Zones in conditional graph"){

    //val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val bench = new InvBench(verbose,List(ToMax()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val nbCentroids = 10L

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"openConditions")
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbCentroids,"centroids")

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

    //val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    for(i <- 0 until 1000){
      val bench = new InvBench(verbose,List(ToMax()))

      val nbNodes = 50
      val nbConditionalEdges = 50
      val nbNonConditionalEdges = 50
      val nbTarget = 10

      val nbCentroids = 10L

      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges, name="openConditions")
      val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbCentroids, name = "Centroids")

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
  }
}
