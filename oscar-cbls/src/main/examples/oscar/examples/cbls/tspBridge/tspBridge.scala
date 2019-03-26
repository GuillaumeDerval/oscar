package oscar.examples.cbls.tspBridge

import oscar.cbls.{CBLSIntVar, CBLSModel, Objective, SeqValue, SetValue, Store, length, profile}
import oscar.cbls.algo.graph.{ConditionalGraph, DijkstraDistanceMatrix, RandomGraphGenerator}
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing.{VRP, insertPointUnroutedFirst, onePointMove}
import oscar.cbls.business.routing.invariants.RouteLengthOnConditionalGraph
import oscar.cbls.core.search.First
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.modeling.ModelingAPI
import oscar.examples.cbls.wlpGraph.WarehouseAndBridgeLocation._

object TspBridge extends ModelingAPI{

  val n = 100
  val nbNodes = 1000
  val nbConditionalEdges = 200
  val nbNonConditionalEdges = 1000
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
  startWatch()
  val underApproximatingDistanceInGraphAllBridgesOpen:Array[Array[Long]] = DijkstraDistanceMatrix.buildDistanceMatrix(graph, _ => true)
  println("end dijkstra")

  val m = Store()
  println("model")

  //initially all bridges open
  val bridgeConditionArray = Array.tabulate(nbConditionalEdges)(c => CBLSIntVar(m, 1, 0 to 1, "bridge_" + c + "_open"))

  val openBridges = Filter(bridgeConditionArray).setName("openBridges")

  val costPerBridge = 200

  val bridgeCost = cardinality(openBridges) * costPerBridge
  val myVRP = new VRP(m,n,1)


  val routeLength = RouteLengthOnConditionalGraph(
    myVRP.routes,
    n = n,
    v = 1,
    openConditions = openConditions,
    nodeInRoutingToNodeInGraph = identity, //we keep it simple for this random example
    graph = graph,
    underApproximatingDistance = (a,b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b),
    distanceIfNotConnected = Int.MaxValue)(0)


  val penaltyForUnrouted  = 10000

  val obj = Objective(routeLength + bridgeCost + (penaltyForUnrouted*(n - length(myVRP.routes))))

  m.close()

  val routedPostFilter = (node:Long) => (neighbor:Long) => myVRP.isRouted(neighbor)

  //this is an array, that, for each node in the routing problem,
  // keeps the sorted closest other point in the routing problem
  val closestRoutingPoint:Array[Iterable[Long]] = Array.tabulate(n)((nodeInGraph:Int) =>
    KSmallest.lazySort(
      Array.tabulate(n)(i => i),
      (otherNode:Long) => underApproximatingDistanceInGraphAllBridgesOpen(nodeInGraph)(otherNode.toInt)
    ))

  // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
  def routeUnroutedPoint(k:Int) =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(k,closestRoutingPoint((x:Long) => x.toInt),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = First())) // Inserting after the first node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k:Long) = profile(onePointMove(
    myVRP.routed,
    ()=>myVRP.kFirst(20,closestRoutingPoint((x:Long) => x.toInt),routedPostFilter),
    myVRP))

  def switchBridge = profile(assignNeighborhood(bridgeConditionArray,"switchBridge"))

  val search = bestSlopeFirst(List(routeUnroutedPoint(20),onePtMove(20),switchBridge))

  search.verbose = 2

  search.doAllMoves(obj = obj)
}
