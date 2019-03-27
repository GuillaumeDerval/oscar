package oscar.examples.cbls.tspBridge

import oscar.cbls._
import oscar.cbls.algo.graph.{DijkstraDistanceMatrix, RandomGraphGenerator}
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing.invariants.RouteLengthOnConditionalGraph
import oscar.cbls.business.routing.neighborhood.OnePointMove
import oscar.cbls.business.routing.{VRP, insertPointUnroutedFirst}
import oscar.cbls.core.computation.CBLSIntConst
import oscar.cbls.core.search.{Best, First, JumpNeighborhood}
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.seq.Length
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods.AssignNeighborhood

import scala.language.implicitConversions

object TspBridge extends App {

  val n = 100
  val nbNodes = 1000
  val nbConditionalEdges = 1000
  val nbNonConditionalEdges = 2000
  val nbTransitNodes = (0.9 * nbNodes).toInt

  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=nbNodes,
    nbConditionalEdges = nbConditionalEdges,
    nbNonConditionalEdges = nbNonConditionalEdges,
    nbTransitNodes = nbTransitNodes,
    mapSide = 1000)

  println("end generate random graph")


  println("start dijkstra")

  val underApproximatingDistanceInGraphAllBridgesOpen:Array[Array[Long]] = DijkstraDistanceMatrix.buildDistanceMatrix(graph, _ => true)
  println("end dijkstra")

  val m = Store() //checker = Some(new ErrorChecker()))
  println("model")

  //initially all bridges open
  val bridgeConditionArray = Array.tabulate(nbConditionalEdges)(c => CBLSIntVar(m, 1, 0 to 1, "bridge_" + c + "_open"))

  val openBridges = Filter(bridgeConditionArray).setName("openBridges")

  val costPerBridge = 20

  val bridgeCost:IntValue = Cardinality(openBridges) * costPerBridge
  val myVRP = new VRP(m,n,1)


  val routeLength:IntValue = RouteLengthOnConditionalGraph(
    myVRP.routes,
    n = n,
    v = 1,
    openConditions = openBridges,
    nodeInRoutingToNodeInGraph = identity, //we keep it simple for this random example
    graph = graph,
    underApproximatingDistance = (a,b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b),
    distanceIfNotConnected = Int.MaxValue/10)(0)


  val penaltyForUnrouted  = 1000L

  val obj:Objective = routeLength + bridgeCost - Length(myVRP.routes) * penaltyForUnrouted + CBLSIntConst(n * penaltyForUnrouted)

  m.close()


  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // visu
  //TODO

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  val routedPostFilter = (node:Long) => (neighbor:Long) => myVRP.isRouted(neighbor)

  //this is an array, that, for each node in the routing problem,
  // keeps the sorted closest other point in the routing problem
  val closestRoutingPoint:Array[Iterable[Long]] = Array.tabulate(n)((nodeInGraph:Int) =>
    KSmallest.lazySort(
      Array.tabulate(n)(i => i),
      (otherNode:Long) => underApproximatingDistanceInGraphAllBridgesOpen(nodeInGraph)(otherNode.toInt)
    ))

  // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
  def routeUnroutedPoint(k:Int) =  Profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(k,(x:Long) =>closestRoutingPoint(x.toInt),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = First())) // Inserting after the first node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k:Long) = Profile(OnePointMove(
    myVRP.routed,
    ()=>myVRP.kFirst(20,(x:Long) =>closestRoutingPoint( x.toInt),routedPostFilter),
    myVRP))

  def switchBridge = Profile(AssignNeighborhood(bridgeConditionArray,"switchBridge"))

  val search = (BestSlopeFirst(List(
    routeUnroutedPoint(20),
    onePtMove(20)),refresh = 20)
    onExhaust (() => {println("finished inserts")})
    exhaust (BestSlopeFirst(List(
    onePtMove(20),
    switchBridge),refresh = 20)
    onExhaustRestartAfter(new JumpNeighborhood("OpenAllBridges"){
    override def doIt(): Unit = {
      for(bridge <- bridgeConditionArray.indices){
        bridgeConditionArray(bridge) := 1
      }
    }
  },maxRestartWithoutImprovement = 2, obj)))

  search.verbose = 1

  search.doAllMoves(obj = obj)

  println(search.profilingStatistics)

  println(myVRP)
  println(openBridges)
}
