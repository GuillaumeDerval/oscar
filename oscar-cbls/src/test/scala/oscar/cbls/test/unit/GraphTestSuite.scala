package oscar.cbls.test.unit

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.{CBLSSetVar, SetValue}
import oscar.cbls.algo.graph.{FloydWarshall, RandomGraphGenerator}
import oscar.cbls.lib.invariant.graph.{DistanceInConditionalGraph, VoronoiZones}
import oscar.cbls.test.invariants.bench._

class GraphTestSuite extends FunSuite with Checkers {

  test("distance in conditional graph"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 100
    val  nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges)

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbTarget)(
      targetID => new DistanceInConditionalGraph(graph,
        from = nbTarget,
        to = targetID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.run()
  }

  test("distance in conditional graph with non transit nodes"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 100
    val  nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges)

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbTarget)(
      targetID => new DistanceInConditionalGraph(graph,
        from = nbTarget,
        to = targetID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.run()
  }

  test("Voronoi Zones in conditional graph"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val nbCentroids = 10L

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges)
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbCentroids)


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

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges)
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbCentroids)


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
