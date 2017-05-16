package oscar.cbls.business.routing.model

import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, IntValue, Store}
import oscar.cbls.lib.invariant.logic.{Cluster, DenseCluster, IntInt2Int}
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.numeric.Div
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

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
  * @param timeLimit the maximum time value supported in seconds (by default 2 days => 172800 s)
  * @param maxPivotPerValuePercent
  */
class PDPv2 (override val n:Int,
             override val v:Int,
             override val m:Store,
             val chains:Array[Array[Int]],
             val timeLimit:Int = 172800,
             maxPivotPerValuePercent:Int = 4)
  extends VRP(n,v,m,maxPivotPerValuePercent) with NextAndPrev{

  // The chain of each node
  val chainOfNode:Array[Array[Int]] = Array.tabulate(n-v)(_ => Array.empty)

  /**
    * This array represents the next node of each node (in his chain).
    */
  val nextNode:Array[Option[Int]] = Array.tabulate(n)(_ => None)

  /**
    * This array represents the previous node of each node (in his chain).
    */
  val prevNode:Array[Option[Int]] = Array.tabulate(n)(_ => None)


  /**
    * This method is used to set the prevStep/nextStep value of each node contained in a chain.
    * The prevStep of the first node is himself and the nextStep of the last node is himself.
    * @param chain the chain
    */
  private def setPrevNext(chain: Array[Int]){
    for(i <- chain.indices){
      val node = chain(i)
      if (i > 0)
        prevNode(node) = Some(chain(i-1))
      if (i < chain.length-1)
        nextNode(node) = Some(chain(i+1))
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
  def unroutedPickups={
    unroutedChains.map(_.head)
  }

  /**
    * @return An array of routed Pickups
    */
  def routedPickups={
    routedChains.map(_.head)
  }

  def pickupOfChain(chain: Int) = chains(chain).head
  def isPickup(node: Int) = chainOfNode(node).head == node
  def getRelatedPickup(node: Int) = chainOfNode(node).head

  def deliveryOfChain(chain: Int) = chains(chain).last
  def isDelivery(node: Int) = chainOfNode(node).last == node
  def getRelatedDelivery(node: Int) = chainOfNode(node).last

  /**
    * @param node The node
    * @return The nodes between the previous node (in chain) of node
    *         and the next node (in chain) of node
    */
  def relevantNewPredecessorsOf(node: Int) = getNodesBetween(prevNode(node),nextNode(node))


  /**
    * @param from The left border (inclusive)
    * @param to The right border (exclusive)
    * @return preceding nodes of node int the route
    */
  def getNodesBetween(from: Option[Int], to: Option[Int]): Iterable[Int] ={
    require(from.isDefined || to.isDefined, "Either from or to must be defined !")
    def buildList(node: Int, betweenList: List[Int]): List[Int] ={
      if(node == to.getOrElse(-1) || (node < v && node != from.get)) return betweenList
      buildList(next(node).value, List(node) ++ betweenList)
    }
    buildList(from.getOrElse(next(getVehicleOfNode(to.get)).value), List.empty)
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
    val route = getRouteOfVehicle(routeNumber)
    /**
      * Each value of segmentsArray represent a possible complete segment.
      * The Int value represents the amount of pickup nodes whose related delivery node isn't currently in the segment
      * The List[Int] value represents the segment
      */
    var pickupInc = 0
    val segmentsArray:Array[List[Int]] = Array.tabulate(chains.length)(_ => List.empty)
    var completeSegments: List[(Int, Int)] = List.empty

    for(node <- route) {
      for (j <- 0 to pickupInc if segmentsArray(j) != null){
        if (isPickup(node)) {
          //If the node is a pickup one, we add the node to all the active segment and the one at position route(i)
          segmentsArray(j) = segmentsArray(j) :+ node
          pickupInc += 1
        }
        else if (isDelivery(node)) {
          /**
            * If the segment doesn't contain the related pickup node it means that the related pickup node is before
            * the beginning of the segment and thus this is not possible to create a complete segment beginning
            * at this position.
            */
          if (!segmentsArray(j).contains(getRelatedPickup(route(node))))
            segmentsArray(j) = null
          /**
            * Else we decrement the number of single pickup
            */
          else {
            segmentsArray(j) = segmentsArray(j) :+ route(node)
            //TODO : Check if this solution wroks properly. It should.
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

  def setVehicleMaxCapacities(maxCapacities: Array[Int]) = vehiclesMaxCapacities.map(maxCapacities(_))

  /**
    * This method is used to set the content flow of each node except vehicle ones.
    * @param contents An array that contains the content flow of each node (except vehicle ones)
    */
  def defineContentsFlow(contents: Array[Int]): Unit ={
    require(contents.length == n,
      "Contents must have the size of the number of nodes (n)." +
        "\nn = " + (n) + ", contents's size : " + contents.length)
    contentsFlow.map(n => contents(n))
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

  var arrivalTimes:Array[CBLSIntVar] = Array.empty

  var leaveTimes:Array[CBLSIntVar] = Array.empty

  var travelOutDurations:Array[CBLSIntVar] = Array.empty

  var arrivalTimesToNext:Array[IntValue] = Array.empty

  var arrivalTimeCluster: DenseCluster[IntValue] = _

  var travelDurationMatrix: TravelTimeFunction = _

  var waitingDurations: Array[IntValue] = Array.empty


  //TODO Int or Option[Int] (in case we don't want to specify anything)
  def addTimeWindows(timeWindows: Array[(Int,Int,Int,Int)]): Unit ={
    require(timeWindows.length == n, "You must specified vehicles and nodes timeWindows.\n" +
      " TimeWindows supposed size : " + n + " , actual size : " + timeWindows.length)

    initiateTimeWindowInvariants()
    addTimeWindowStringInfo()

    for(i <- timeWindows.indices){
      earlylines(i) = timeWindows(i)._1
      deadlines(i) = timeWindows(i)._2
      taskDurations(i) = timeWindows(i)._3
      maxWaitingDurations(i) = timeWindows(i)._4

      if(earlylines(i) == 0)
        setNodeDuration(i + v, timeWindows(i)._3)
      else
        setNodeDuration(i + v, timeWindows(i)._3, timeWindows(i)._1)
    }

    waitingDurations = Array.tabulate(n){
      (i:Int) =>
        if(i >= v && maxWaitingDurations(i) != Int.MaxValue)
          Max2(leaveTimes(i) - taskDurations(i) - arrivalTimes(i), CBLSIntConst(0))
        else
          new CBLSIntConst(0)
    }
  }


  def initiateTimeWindowInvariants(): Unit ={
    val defaultArrivalTime = new CBLSIntConst(0)
    val clusterSizeInSec = 900

    arrivalTimes = Array.tabulate(n) {
      (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "arrivalTimeAtNode" + i)
    }
    leaveTimes = Array.tabulate(n) {
      (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "leaveTimeAtNode" + i)
    }
    travelOutDurations = Array.tabulate(n) {
      (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / n, "travelDurationToLeave" + i)
    }
    arrivalTimesToNext = Array.tabulate(n + 1) {
      (i: Int) =>
        if (i == n) defaultArrivalTime
        else travelOutDurations(i) + leaveTimes(i)
    }

    for (i <- 0 until n) {
      arrivalTimes(i) <== arrivalTimesToNext.element(prev(i))
    }

    arrivalTimeCluster = Cluster.MakeDenseAssumingMinMax(leaveTimes.map(x => Div(x,clusterSizeInSec)),0,timeLimit/clusterSizeInSec)
  }

  def setNodeDuration(node: Int, duration: IntValue) {
    assert(node >= v)
    leaveTimes(node) <== (arrivalTimes(node) + duration)
  }

  def setNodeDuration(node: Int, duration: IntValue, startWindow: Int) {
    leaveTimes(node) <== (Max2(arrivalTimes(node), startWindow) + duration)
  }

  def setTravelTimeFunctions(travelCosts: TravelTimeFunction) {
    travelDurationMatrix = travelCosts
    for (i <- 0 until n) {
      travelOutDurations(i) <== new IntInt2Int(leaveTimes(i), next(i),
        (leaveTime, successor) =>
          if (successor == n) 0
          else travelCosts.getTravelDuration(i, leaveTime, successor))
    }
  }

  def addTimeWindowStringInfo() {
    addToStringInfo(() => "arrivalTime:      " + arrivalTimes.toList.mkString(","))
    addToStringInfo(() => "leaveTime:        " + leaveTimes.toList.mkString(","))
    addToStringInfo(() => "travelOutDuration:" + travelOutDurations.toList.mkString(","))
    addToStringInfo(() => "arrivalTimeToNext:" + arrivalTimesToNext.toList.mkString(","))
  }

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    */
  def computeClosestNeighborInTime(filter : ((Int,Int) => Boolean) = (_,_) => true): Array[Iterable[Int]] ={
    Array.tabulate(n)(node => {
      val nodeCluster = arrivalTimeCluster.values(node).value
      val res:ListBuffer[Int] = new ListBuffer[Int]()
      for(i <- 0 to nodeCluster)
        for(neighbor <- arrivalTimeCluster.clusters(i).value.toList if isRouted(neighbor) && filter(n,neighbor))
          if(leaveTimes(neighbor).value+travelDurationMatrix.getTravelDuration(neighbor, 0, node) < deadlines(node)) {
            res.append(neighbor)
          }
      res.reverse.toList
    })
  }
}