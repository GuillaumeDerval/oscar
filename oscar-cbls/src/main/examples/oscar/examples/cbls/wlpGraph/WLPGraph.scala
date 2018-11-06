/**
  * *****************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  * ****************************************************************************
  */

package oscar.examples.cbls.wlpGraph

import oscar.cbls._
import oscar.cbls.algo.graph.{ConditionalGraph, ConditionalGraphWithIntegerNodeCoordinates, Node, NodeWithIntegerCoordinates}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.lib.invariant.graph._
import oscar.cbls.lib.invariant.graph.test.RandomGraphGenerator
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.ColorGenerator

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.swing.Color

object WLPGraph extends App with StopWatch{

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 1000

  val c:Int = 200

  val displayDelay = 100

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=(W+D),
    nbConditionalEdges=c,
    nbNonConditionalEdges=(W+D)*5,
    mapSide = 1000)


  //println("floyd")
  //val distanceMatrix = FloydWarshall.buildDistanceMatrix(graph, _ => true):Array[Array[Option[Int]]]

  //warehouses are numbered by nodeID from 0 to W-1
  //shops are numbered by ndeID from W to W+D-1
  val warehouseToNode =  Array.tabulate(W)(w => graph.nodeswithCoordinates(w))
  val deliveryToNode = Array.tabulate(D)(d => graph.nodeswithCoordinates(d + W))

  val deliveryNodeList = QList.buildFromIterable(deliveryToNode).asInstanceOf[QList[Node]]

  def distance(node1:NodeWithIntegerCoordinates,node2:NodeWithIntegerCoordinates):Int = {
    val dx = node1.x - node2.x
    val dy = node1.y - node2.y
    math.sqrt(dx*dx + dy*dy).floor.toInt
  }

  //val warehouseToWarehouseDistances = Array.tabulate(W)(w1 => Array.tabulate(W)(w2 => distanceMatrix(w1)(w2).getOrElse(1000)))
  val warehouseToWarehouseDistances = Array.tabulate(W)(w1 => Array.tabulate(W)(w2 => distance(warehouseToNode(w1),warehouseToNode(w2))))


  val costForOpeningWarehouse =  Array.fill(W)(800)

  val m = Store() //checker = Some(new ErrorChecker()))
  println("model")

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")


  val edgeConditionArray = Array.tabulate(c)(c => CBLSIntVar(m, 1, 0 to 1, "edgeCondition_" + c + "_open"))

  val openConditions = Filter(edgeConditionArray).setName("openConditions")

  println("creating Voronoï zones invariant")

  val vor = VoronoiZones(graph,
    graphDiameterOverApprox = Int.MaxValue,
    openConditions = openConditions,
    centroids = openWarehouses,
    trackedNodes = deliveryToNode.map(_.nodeId),
    m,
    defaultDistanceForUnreachableNodes = 10000)

  val trackedNodeToDistanceAndCentroid: SortedMap[Int,(CBLSIntVar,CBLSIntVar)] = vor.trackedNodeToDistanceAndCentroidMap


  println("done init voronoï zones")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    trackedNodeToDistanceAndCentroid(deliveryToNode(d).nodeId)._1)

  val costOfPassage = 10

  val x = Cardinality(openConditions)
  val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses) + (x*costOfPassage))

  m.close()

  val centroidColors = ColorGenerator.generateRandomColors(W)

  val visual = new ConditionalGraphAndVoronoiZonesMapWindow(graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor = SortedMap.empty[Int,Color] ++ warehouseToNode.toList.map(node => (node.nodeId,centroidColors(node.nodeId))),title = "Warehouse and new road location")

  var bestDisplayedObj = Int.MaxValue

  //this is an array, that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
  val closestWarehouses = Array.tabulate(W)(warehouse =>
    KSmallest.lazySort(
      Array.tabulate(W)(warehouse => warehouse),
      otherWarehouse => warehouseToWarehouseDistances(warehouse)(otherWarehouse)
    ))

  //this procedure returns the k closest closed warehouses
  def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)
  //this procedure returns the k closest open warehouses
  def kNearestOpenWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)
  def kNearestdWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse))

  def muLine(depth:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.head.id
      val setTo = assignList.head.v
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = depth,
    intermediaryStops = true)


  def muStar(width:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.last.id
      val setTo = assignList.head.v
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = width,
    intermediaryStops = true)

  def swapsK(k:Int,openWarehoueseTocConsider:()=>Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
    searchZone1 = openWarehoueseTocConsider,
    searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
    name = "Swap" + k + "Nearest",
    symmetryCanBeBrokenOnIndices = false)

  def doubleSwap(k:Int) = (swapsK(k) dynAndThen((firstSwap:SwapMove) => swapsK(k,() => kNearestOpenWarehouses(firstSwap.idI,k)))) name "DoubleSwap"

  var lastDisplay = this.getWatch

  val neighborhood =(
    BestSlopeFirst(
      List(
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
        Profile(AssignNeighborhood(edgeConditionArray, "SwitchConditions")),
        Profile(SwapsNeighborhood(edgeConditionArray, "SwapConditions")),
        Profile(swapsK(20) guard(() => openWarehouses.value.size >= 5)), //we set a minimal size because the KNearest is very expensive if the size is small
        Profile((swapsK(20) andThen AssignNeighborhood(edgeConditionArray, "SwitchConditions")) guard(() => openWarehouses.value.size >= 5) name "combined"), //we set a minimal size because the KNearest is very expensive if the size is small
        Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") guard(() => openWarehouses.value.size >= 5))
      ),refresh = W/10)
      onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => openWarehouses.value.size/5), 4, obj)
    ) afterMove(
    if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {
      bestDisplayedObj = obj.value

      visual.redraw(
        openConditions.value,
        openWarehouses.value,
        trackedNodeToDistanceAndCentroid.mapValues({ case (v1, v2) => (v2.value) }),
        hideClosedEdges = false,
        hideRegularEdges = false,
        hideOpenEdges = false,
        emphasizeEdges = vor.spanningTree(deliveryNodeList))

      lastDisplay = this.getWatch
    })

  neighborhood.verbose = 2

  neighborhood.doAllMoves(obj=obj)

  visual.redraw(openConditions.value,openWarehouses.value,
    trackedNodeToDistanceAndCentroid.mapValues({case (v1,v2) => (v2.value)}),hideClosedEdges = true,emphasizeEdges = vor.spanningTree(deliveryNodeList),hideRegularEdges = true,hideOpenEdges=false)

  println(neighborhood.profilingStatistics)

  println(openWarehouses)
  println(openConditions)
}

