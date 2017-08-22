package oscar.cbls.business.routing.model

import oscar.cbls.algo.seq.functional.IntSequenceExplorer
import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.routing.MovingVehicles
import oscar.cbls.lib.invariant.routing.capa.{ForwardCumulativeIntegerIntegerDimensionOnVehicle, ForwardCumulativeConstraintOnVehicle}
import oscar.cbls.lib.invariant.seq.SortSequence
import oscar.cbls.lib.invariant.set.{Diff, IncludedSubsets, ValuesInViolatedClauses}

import scala.collection.SortedSet
import scala.collection.immutable.{HashMap, List}
import scala.math._

/**
  * Created by fg on 28/04/17.
  */
/**
  * This class represent a Pickup and Delivery Problem.
  * It's divide in 3 mains parts.
  *   1° A structure of chains containing multiple nodes that represents the steps of the drive
  *   2° A structure representing the vehicle max capacities
  *   3° A structure representing the timeWindows, arrival time, leave time, ... of each nodes
  * @param n the number of points (deposits and customers) in the problem.
  * @param v the number of vehicles.
  * @param m the model.
  * @param chains the chains (drives)
  * @param maxPivotPerValuePercent
  */
class PDP(override val n:Int,
          override val v:Int,
          override val m:Store,
          val chains:List[List[Int]],
          maxPivotPerValuePercent:Int = 4)
  extends VRP(n,v,m,maxPivotPerValuePercent) with NextAndPrev{

  // The chain of each node
  val chainOfNode:Array[List[Int]] = Array.tabulate(n)(_ => List.empty)

  /**
    * This array represents the next node of each node (in his chain).
    */
  val nextNode:Array[Option[Int]] = Array.tabulate(n)(_ => None)

  /**
    * This array represents the previous node of each node (in his chain).
    */
  val prevNode:Array[Option[Int]] = Array.tabulate(n)(_ => None)

  val nextNodesInChains = chains.flatMap(c => {
    def generateDico(currentList: List[Int], dico : List[(Int,List[Int])]): List[(Int,List[Int])] ={
      if(currentList.isEmpty)
        dico
      else
        generateDico(currentList.tail, List(currentList.head -> currentList.tail) ++ dico)
    }
    generateDico(c, List.empty)
  }).toMap

  val prevNodesInChains = chains.flatMap(c => {
    def generateDico(currentList: List[Int], dico : List[(Int,List[Int])]): List[(Int,List[Int])] ={
      if(currentList.isEmpty)
        dico.map(x => (x._1,x._2.reverse))
      else
        generateDico(currentList.tail, List(currentList.head -> currentList.tail) ++ dico)
    }
    generateDico(c.reverse, List.empty)
  }).toMap

  var exclusiveCarsSubsets: IncludedSubsets = _

  var availableVehicles: SetValue = _

  val movingVehicles: MovingVehicles = new MovingVehicles(routes,v)

  for(chain <- chains) {
    for (i <- chain.indices) {
      val node = chain(i)
      if (i > 0)
        prevNode(node) = Some(chain(i - 1))
      if (i < chain.length - 1)
        nextNode(node) = Some(chain(i + 1))
      chainOfNode(node) = chain
    }
  }

  /**
    * @return An array of unrouted Drives
    */
  def unroutedChains={
    chains.filter(c => !isRouted(c.head))
  }

  /**
    * @return An array of routed Drives
    */
  def routedChains={
    chains.filter(c => isRouted(c.head))
  }

  /**
    * @return An array of unrouted Pickups
    */
  def unroutedPickups: Iterable[Int]={
    unroutedChains.map(_.head)
  }

  /**
    * @return An array of routed Pickups
    */
  def routedPickups: Iterable[Int]={
    routedChains.map(_.head)
  }

  /**
    * @param ord A method that set the order of the pickup
    * @return A list of all the pickups ordered by ord
    */
  def orderPickups(ord : (Int) => Int): Iterable[Int] = {
    chains.map(_.head).sortBy(ord(_))
  }


  def isPickup(node: Int) = node >= v && chainOfNode(node).head == node
  def getRelatedPickup(node: Int) = {
    require(node >= v, "This node is a depot !")
    chainOfNode(node).head
  }

  def isDelivery(node: Int) = node >= v && chainOfNode(node).last == node
  def getRelatedDelivery(node: Int) = {
    require(node >= v, "This node is a depot !")
    chainOfNode(node).last
  }

  /**
    * @param node The node
    * @return The nodes between the previous node (in chain) of node
    *         and the next node (in chain) of node
    */
  def relevantNewPredecessorsOf()(node: Int) = getNodesBetween(prevNode(node),nextNode(node))


  /**
    * @param from The left border (inclusive)
    * @param to The right border (exclusive)
    * @return preceding nodes of node int the route
    */
  def getNodesBetween(from: Option[Int], to: Option[Int]): Iterable[Int] ={
    require(from.isDefined || to.isDefined, "Either from or to must be defined !")
    def buildList(node: Int, betweenList: List[Int]): List[Int] ={
      if(node == to.getOrElse(-1) || (node < v && node != from.getOrElse(getVehicleOfNode(to.get))) || node == n) return betweenList
      buildList(next(node).value, List(node) ++ betweenList)
    }
    buildList(from.getOrElse(getVehicleOfNode(to.get)), List.empty)
  }

  def addExclusiveCarsSubsests(exclusiveCars: List[List[Int]]): Unit ={
    exclusiveCarsSubsets = IncludedSubsets(
      new MovingVehicles(routes,v),
      exclusiveCars.map(carList => (carList,1,1))
    )
    availableVehicles = Diff(CBLSSetConst(SortedSet.empty[Int] ++ (0 to v-1)),Diff(ValuesInViolatedClauses(
      movingVehicles,
      exclusiveCars.map(carList => (carList,0))
    ), movingVehicles))
  }


  /**
    * This method search all the complete segments contained in a specified route.
    * A segment is considered as complete when you can move it to another place
    * without breaking the precedence constraint.
    * It runs through the specified route and try to create the smallest complete segments possible
    * After that it try to combine adjacent segment
    *
    * @param routeNumber the number of the route
    * @return the list of all the complete segment present in the route
    */
  def getCompleteSegments(routeNumber:Int): List[(Int,Int)] ={
    val route = getRouteOfVehicle(routeNumber).drop(1)
    /**
      * Each value of segmentsArray represent a possible complete segment.
      * The List[Int] value represents the segment
      */
    var pickupInc = 0
    val segmentsArray:Array[List[Int]] = Array.tabulate(chains.length)(_ => List.empty)
    var completeSegments: List[(Int, Int)] = List.empty

    for(node <- route) {
      if(isPickup(node)) pickupInc += 1
      for (j <- 0 until pickupInc if segmentsArray(j) != null){
        if (isPickup(node)) {
          //If the node is a pickup one, we add the node to all the active segment and the one at position route(i)
          segmentsArray(j) = segmentsArray(j) :+ node
        }
        else if (isDelivery(node)) {
          /**
            * If the segment doesn't contain the related pickup node it means that the related pickup node is before
            * the beginning of the segment and thus this is not possible to create a complete segment beginning
            * at this position.
            */
          if (!segmentsArray(j).contains(getRelatedPickup(node)))
            segmentsArray(j) = null
           /**
            * Else we decrement the number of single pickup
            */
          else {
            segmentsArray(j) = segmentsArray(j) :+ node
            if (segmentsArray(j).length == 2*(pickupInc-j))
              completeSegments = List((segmentsArray(j).head, segmentsArray(j).last)) ++ completeSegments
          }
        }
      }
    }
    completeSegments
  }


  // --------------------------------- Capacities -------------------------------------- //

  val vehiclesMaxCapacities: Array[Int] = Array.tabulate(v)(_ => 0)

  /**
    * This array contains the content flow of each node of the problem.
    * At each node we can either load/unload article/passenger or do nothing.
    * If the value is positive => load, negative => unload, zero => do nothing.
    */
  val contentsFlow:Array[Int] = Array.tabulate(n)(_ => 0)

  val contentAtNode = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity " + "Content at node")

  var contentConstraint: ForwardCumulativeConstraintOnVehicle = _


  def setVehicleMaxCapacities(maxCapacities: Array[Int]) =
    for(i <- vehiclesMaxCapacities.indices) vehiclesMaxCapacities(i) = maxCapacities(i)

  /**
    * This method is used to set the content flow of each node except vehicle ones.
    * @param contents An array that contains the content flow of each node (except vehicle ones)
    */
  def defineContentsFlow(contents: Array[Int]): Unit ={
    require(contents.length == n,
      "Contents must have the size of the number of nodes (n)." +
        "\nn = " + n + ", contents's size : " + contents.length)
    val vehicleMaxCapacity = vehiclesMaxCapacities.max
    for(i <- contents.indices) {
      contentsFlow(i) = contents(i)
    }

    contentConstraint = new ForwardCumulativeConstraintOnVehicle(routes,n,v,
      (from,to,fromContent) => fromContent + contentsFlow(to),
      vehiclesMaxCapacities.max,
      vehiclesMaxCapacities.map(vehiclesMaxCapacities.max-_),
      contentAtNode,
      chains.map(_.length).max*2,
      "Content at node")
  }





  // --------------------------------- Time ---------------------------------------- //

  // The time at which we can start loading/unloading the vehicle
  val earlylines = Array.tabulate(n)(_ => 0)
  // The time before which we must have started loading/unloading the vehicle
  val deadlines = Array.tabulate(n)(_ => Int.MaxValue)
  // The duration of the task
  val taskDurations = Array.tabulate(n)(_ => 0)
  // The maxWaitingDuration at point
  val maxWaitingDurations = Array.tabulate(n)(_ => Int.MaxValue)

  var arrivalTimes:Array[CBLSIntVar] = _
  var leaveTimes:Array[CBLSIntVar] = _
  var arrivalTimesAtEnd:Array[CBLSIntVar] = _
  var leaveTimesAtEnd:Array[CBLSIntVar] = _
  var lastPointOfVehicles:Array[CBLSIntVar] = _

  var sortedRouteByEarlylines: SortSequence = null

  var maxDetours:Map[Int,(Int,Int)] = HashMap.empty

  var travelDurationMatrix: TravelTimeFunction = _

  //TODO Int or Option[Int] (in case we don't want to specify anything)
  def addTimeWindows(timeWindows: Array[(Int,Int,Int,Int)]): Unit ={
    require(timeWindows.length == n, "You must specified vehicles and nodes timeWindows.\n" +
      " TimeWindows supposed size : " + n + " , actual size : " + timeWindows.length)

    for(i <- timeWindows.indices){
      earlylines(i) = timeWindows(i)._1
      deadlines(i) = timeWindows(i)._2
      taskDurations(i) = timeWindows(i)._3
      maxWaitingDurations(i) = timeWindows(i)._4
    }

    val timeInvariant = ForwardCumulativeIntegerIntegerDimensionOnVehicle(
      routes,n,v,
      (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
        val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
        val leaveTimeAtToNode =
          if(toNode < v) 0
          else Math.max(arrivalTimeAtToNode,earlylines(toNode)) + taskDurations(toNode)
        (arrivalTimeAtToNode,leaveTimeAtToNode)
      },
      Array.tabulate(v)(x => new CBLSIntConst(0)),
      Array.tabulate(v)(x => new CBLSIntConst(earlylines(x)+taskDurations(x))),
      0,
      0,
      contentName = "Time at node"
    )

    arrivalTimes = timeInvariant._1
    leaveTimes = timeInvariant._2
    arrivalTimesAtEnd = timeInvariant._3
    leaveTimesAtEnd = timeInvariant._4
    lastPointOfVehicles = timeInvariant._5


    sortedRouteByEarlylines = SortSequence(routes, node => earlylines(node))
  }

  def addMaxDetours(maxDetourCalculation:(List[Int],TravelTimeFunction) => List[(Int,Int,Int)]): Unit ={
    maxDetours = (
      for(chain <- chains)
        yield maxDetourCalculation(chain, travelDurationMatrix)
    ).toArray.flatten.map(x => x._2 -> (x._1,x._3)).toMap

    for(to <- 0 until n if !isPickup(to) && to >= v)
      deadlines(to) = maxDetours.get(to) match{
        case None =>
          nextNode(to) match{
            case None => Int.MaxValue
            case Some(nextN) => Math.min(deadlines(to), deadlines(nextN) - travelDurationMatrix.getTravelDuration(to,0,nextN) - taskDurations(nextN))
          }
        case Some((from,value)) => Math.min(deadlines(to), deadlines(from) + value + taskDurations(to))
      }
    //TODO : Move this, it doesn't belong here !!!
    for(chain <- chains){
      for(node <- chain  if !isPickup(node) && earlylines(node) == 0){
        val previous = prevNode(node).get
        earlylines(node) = earlylines(previous) + taskDurations(previous) +
          travelDurationMatrix.getTravelDuration(previous,earlylines(previous),node)
      }
    }
  }

  def setTravelTimeFunctions(travelCosts: TravelTimeFunction) {
    travelDurationMatrix = travelCosts
  }

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    * @param k  the max number of closestNeighbor we want to inspect
    * @param filter an undefined filter used to filter the neighbor (neighbor,node) => Boolean
    * @param node the node we want to find neighbor for
    * @return the k closest neighbor of the node
    */
  def computeClosestNeighborsInTime(k: Int = Int.MaxValue,
                                    filter: (Int,Int) => Boolean = (_,_) => true
                                  )(node:Int): Iterable[Int] ={
    def buildPotentialNeighbors(explorer: Option[IntSequenceExplorer], potentialNeighbors: List[Int]): List[Int] = {
      if (explorer.isEmpty)
        potentialNeighbors
      else if (explorer.get.value < v && !availableVehicles.value.contains(explorer.get.value))
        buildPotentialNeighbors(explorer.get.prev, potentialNeighbors)
      else
        buildPotentialNeighbors(explorer.get.prev, List(explorer.get.value) ++ potentialNeighbors)
    }
    val explorer = sortedRouteByEarlylines.positionOfSmallestGreaterOrEqual(node)(deadlines(node))
    val potentialNeighbors = (
    if(explorer.isDefined) buildPotentialNeighbors(explorer,List.empty)
    else availableVehicles.value.toList.map(x => prev(x).value)).
      filter(prevNode => if(prevNode < v) vehiclesMaxCapacities(prevNode) >= contentsFlow(node) else true)
    buildClosestNeighbor(
      node,
      potentialNeighbors,  // arrival time of a node is 0 by default.
      filter,
      List.empty[(Int,Int)]
    ).take(k)
  }

  /**
    *
    * @param neighbors
    * @param closestNeighbors
    * @return
    */
  private def buildClosestNeighbor(node: Int,
                                   neighbors: List[Int],
                                   filter: (Int,Int) => Boolean = (_,_) => true,
                                   closestNeighbors: List[(Int,Int)]): List[Int] ={
    if(neighbors.isEmpty)
      return closestNeighbors.sortBy(_._2).map(_._1)
    val neighbor = neighbors.head
    if (filter(neighbor,node)  &&
      leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node) <= deadlines(node)) {
      val nextOfNeighbor = next(neighbor).value
      val neighborToNode = max(leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node), earlylines(node))
      val neighborToNodeToNext = neighborToNode + taskDurations(node) + travelDurationMatrix.getTravelDuration(node, 0, nextOfNeighbor)
      if (neighborToNodeToNext <= deadlines(nextOfNeighbor))
        return buildClosestNeighbor(node, neighbors.tail, filter, List((neighbor,neighborToNodeToNext)) ++ closestNeighbors)
    }
    buildClosestNeighbor(node, neighbors.tail, filter, closestNeighbors)
  }

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    * It filter the clusters to avoid checking neighbor belonging to cluster
    * before the prevNode's cluster or after nextNode's cluster.
    * @param k  the max number of closestNeighbor we want to inspect
    * @param routeOfNode The route we want to find closest neighbor within
    *                    If you use this method many times, try to get the route once and for all
    *                    in the calling method.
    * @param filter an undefined filter used to filter the neighbor (neighbor,node) => Boolean
    * @param node the node we want to find neighbor for
    * @return the k closest neighbor of the node
    */
  def computeClosestNeighborsInTimeInRoute(k: Int = Int.MaxValue,
                                      routeOfNode: Option[List[Int]],
                                      filter: (Int,Int) => Boolean = (_,_) => true)(node:Int): Iterable[Int] ={
    val pickup = getRelatedPickup(node)
    val pickupEarlyLine = earlylines(pickup)
    val neighbors = routeOfNode.getOrElse(getRouteOfVehicle(getVehicleOfNode(node))).dropWhile(leaveTimes(_).value < pickupEarlyLine)
    buildClosestNeighbor(node, neighbors, filter, List.empty).take(k)
  }
}
