package oscar.cbls.test.unit

import org.scalacheck.{Gen, Prop}
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import oscar.cbls.CBLSSetVar
import oscar.cbls.algo.graph._
import oscar.cbls.core.propagation.PropagationElement
import oscar.cbls.lib.invariant.graph.DistanceInConditionalGraph
import oscar.cbls.test.invariants.bench._

class DistanceInConditionalGraphTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers{

  val verbose = 0

  test("DistanceInConditionalGraph maintains the distance between two nodes"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val nbNodes = 100
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"OpenConditions")

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

  test("DistanceInConditionalGraph maintains the distance between two nodes (with transit)"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 100
    val nbConditionalEdges = 50
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


  test("DistanceInConditionalGraph from a node to itself should be 0"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 0, range = 0 to 0)
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes)(
      nodeID => new DistanceInConditionalGraph(graph,
        from = nodeID,
        to = nodeID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.model.close()
    bench.model.propagate()

    forAll(Gen.oneOf(distancesArray)){
      distance :DistanceInConditionalGraph => distance.value should be(0)
    }
  }

  test("DistanceInConditionalGraph from a node to an unreachable node should be 'distanceIfNotConnected'"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 to 0)
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges,
      nbConditions = graphTemp.nbConditions)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes)(
      nodeId => new DistanceInConditionalGraph(graph,
        from = nodeId,
        to = lonelyNode.nodeId,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)(underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    )

    bench.model.close()
    bench.model.propagate()

    forAll(Gen.oneOf(distancesArray)){
      distance :DistanceInConditionalGraph => distance.value should be(Long.MaxValue)
    }
  }

  test("DistanceInConditionalGraph with all edges closed should be 'distanceIfNotConnected'"){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 15
    val nbNonConditionalEdges = 0
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 to nbConditionalEdges)
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes,nbNodes)(
      (nodeId1,nodeId2) => new DistanceInConditionalGraph(graph,
        from = nodeId1,
        to = nodeId2,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))).flatten

    bench.model.close()
    bench.model.propagate()

    forAll(Gen.oneOf(distancesArray)){
      distance :DistanceInConditionalGraph => distance.value should (be(Long.MaxValue) or be(0))
    }
  }

}
