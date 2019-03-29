package oscar.examples.cbls.tspBridge

import java.awt.Color

import oscar.cbls._
import oscar.cbls.algo.graph._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.RouteLengthOnConditionalGraph
import oscar.cbls.core.computation.CBLSIntConst
import oscar.cbls.core.search.{First, JumpNeighborhood}
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.graph.SimpleGraphViewer

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

object TspBridge extends App {

  val n = 100
  val nbNodes = 1000
  val nbConditionalEdges = 500
  val nbNonConditionalEdges = 3000
  val nbTransitNodes = (nbNodes).toInt

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

  val openBridges = filter(bridgeConditionArray).setName("openBridges")

  val costPerBridge = 20

  val bridgeCost:IntValue = cardinality(openBridges) * costPerBridge
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

  val obj:Objective = routeLength + bridgeCost - length(myVRP.routes) * penaltyForUnrouted + CBLSIntConst(n * penaltyForUnrouted)

  m.close()


  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // visu

  val visu = new TspBridgeVisu(graph, v = 1, n,(a,b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b))
  SingleFrameWindow.show(visu,"TspBridge(n" + n + ")")
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
  def routeUnroutedPoint(k:Int) = profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(k,(x:Long) =>closestRoutingPoint(x.toInt),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = First())) // Inserting after the first node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k:Long) = profile(onePointMove(
    myVRP.routed,
    ()=>myVRP.kFirst(20,(x:Long) =>closestRoutingPoint( x.toInt),routedPostFilter),
    myVRP))


  def myThreeOpt(k:Int) = profile(
    threeOpt(potentialInsertionPoints = myVRP.routed,
      relevantNeighbors =()=>myVRP.kFirst(k,(x:Long) =>closestRoutingPoint(x.toInt),routedPostFilter),
      vrp = myVRP))

  def switchBridge = profile(assignNeighborhood(bridgeConditionArray,"switchBridge"))

  val search = (bestSlopeFirst(List(
    routeUnroutedPoint(50),
    myThreeOpt(20),
    onePtMove(20)),refresh = 20)
    onExhaust (() => {println("finished inserts")})
    exhaust (bestSlopeFirst(List(
    onePtMove(40),
    myThreeOpt(20),
    switchBridge),refresh = 10)
    onExhaustRestartAfter(new JumpNeighborhood("OpenAllBridges"){
    override def doIt(): Unit = {
      for(bridge <- bridgeConditionArray.indices){
        bridgeConditionArray(bridge) := 1
      }
    }
  },maxRestartWithoutImprovement = 2, obj))
    afterMove{
    visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList.map(_.toInt), myVRP.routes.value)
  })

  search.verbose = 1

  search.doAllMoves(obj = obj)

  println(search.profilingStatistics)

  println(myVRP)
  println(openBridges)

  visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList.map(_.toInt), myVRP.routes.value)

}


class TspBridgeVisu(graph:ConditionalGraphWithIntegerNodeCoordinates,
                    v:Int,
                    n:Int,
                    underApproximatingDistance:(Int,Int) => Long)
  extends SimpleGraphViewer(graph){

  def redraw(openBridges:SortedSet[Int], routes:IntSequence): Unit ={
    super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble


    //all edges, simple made
    for(edge <- graph.edges){
      drawEdge(edge, 1, Color.black, dashed = true)
    }

    //furthermore, open edges are in Green, cosed edges are in RED
    for(condition <- 0 until graph.nbConditions){
      val conditionalEdge = graph.conditionToConditionalEdges(condition)
      if(openBridges contains condition){
        drawEdge(conditionalEdge, 5, Color.green)
      }else{
        drawEdge(conditionalEdge, 1, Color.pink, dashed = true)
      }
    }

    //pathes
    var currentExplorer = routes.explorerAtAnyOccurrence(0).get

    while(currentExplorer.next match{
      case None => //return
        drawPath(graph.nodes(currentExplorer.value), graph.nodes(0), openBridges)
        false
      case Some(expl) =>
        drawPath(graph.nodes(currentExplorer.value), graph.nodes(expl.value), openBridges)
        currentExplorer = expl
        true
    }){}

    //underlying graph with small nodes, dotted black edges
    for(node <- graph.nodes){
      drawRoundNode(node, Color.BLACK, 1)
    }

    //routing nodes
    for(nodeId <- 0 until n){
      val node = graph.nodes(nodeId)
      if(routes contains nodeId){
        drawRoundNode(node, Color.BLUE, 3)
      }else{
        drawRoundNode(node, Color.RED, 3)
      }
    }

    //double buffering still does not work!
    super.repaint()
  }

  private val aStarEngine = new RevisableAStar(graph: ConditionalGraph, underApproximatingDistance)


  def drawPath(fromNode:Node, toNode:Node, openConditions:SortedSet[Int]): Unit ={
    drawEdges(aStarEngine.getPath(fromNode,toNode,openConditions).get, 2, Color.BLUE)
  }

}