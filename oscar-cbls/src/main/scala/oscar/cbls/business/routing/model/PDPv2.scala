package oscar.cbls.business.routing.model

import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, IntValue, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{GE, LE}
import oscar.cbls.lib.invariant.logic.{Cluster, DenseCluster, IntITE, IntInt2Int}
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.numeric.Div
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
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
  * @param c the number of chains (drives)
  * @param timeLimit the maximum time value supported in seconds (by default 2 days => 172800 s)
  * @param maxPivotPerValuePercent
  */
class PDPv2 (override val n:Int, override val v:Int, override val m:Store, val c:Int, val timeLimit:Int = 172800, maxPivotPerValuePercent:Int = 4)
  extends VRP(n,v,m,maxPivotPerValuePercent) with NextAndPrev{

  /**
    * This list represents a list of chain.
    * Each chain is represented by an array of node (at least 2).
    * Each node represents a step of the chain.
    * The first node is the pickup node and the last node the delivery node
    */
  val chains:Array[Array[Int]] = Array.tabulate(c)(_ => Array.empty)

  // The chain of each node
  val chainOfNode:Array[Array[Int]] = Array.tabulate(n-v)(_ => Array.empty)

  /**
    * This array represents the next node of each node (in his chain).
    */
  val nextNode:Array[Int] = Array.tabulate(n)(_ => 0)

  /**
    * This array represents the previous node of each node (in his chain).
    */
  val prevNode:Array[Int] = Array.tabulate(n)(_ => 0)

  /**
    * This method is used to set the chains of the problem.
    * @param chains array list of chains
    */
  def addDrives(chains: Array[Array[Int]]): Unit ={
    require(chains.length == c, "The chains length must have the same value as c (the number of chain). " +
      "\nchains.length : " + chains.length + "    number of chains : " + c)
    for(i <- chains.indices) {
      val drive = chains(i)
      this.chains(i) = drive
      setPrevNext(drive)
    }
  }

  /**
    * This method is used to set the prevStep/nextStep value of each node contained in a chain.
    * The prevStep of the first node is himself and the nextStep of the last node is himself.
    * @param chain the chain
    */
  private def setPrevNext(chain: Array[Int]){
    for(i <- chain.indices){
      val node = chain(i)
      prevNode(node) = if(i == 0) node else chain(i-1)
      nextNode(node) = if(i == chain.length-1) node else chain(i+1)
      chainOfNode(node) = chain
    }
  }

  /**
    * @return An array of unrouted Drives
    */
  def unroutedDrives={
    chains.filter(c => isRouted(c.head))
  }

  def pickupOfChain(chain: Int) = chains(chain).head
  def isPickup(node: Int) = chainOfNode(node).head == node
  def getRelatedPickup(node: Int) = chainOfNode(node).head

  def deliveryOfChain(chain: Int) = chains(chain).last
  def isDelivery(node: Int) = chainOfNode(node).last == node
  def getRelatedDelivery(node: Int) = chainOfNode(node).last




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
  // TODO improve this method, it seems to have to much loop
  def getCompleteSegments(routeNumber:Int): List[(Int,Int)] ={
    val route = getRouteOfVehicle(routeNumber)
    /**
      * Each value of segmentsArray represent a possible complete segment.
      * The Int value represents the amount of pickup nodes whose related delivery node isn't currently in the segment
      * The List[Int] value represents the segment
      */
    val segmentsArray:Array[(Int,List[Int])] = new Array[(Int, List[Int])](c)

    for(i <- route.indices){
      if(isPickup(route(i))){
        //If the node is a pickup one, we add the node to all the active segment and we create a new one
        for(j <- segmentsArray.indices)
          if(segmentsArray(j) != null)
            if (segmentsArray(j)._1 != 0)
              segmentsArray(j) = (segmentsArray(j)._1 + 1, segmentsArray(j)._2 :+ route(i))
        segmentsArray(i) = (1,route(i)::Nil)
      }
      else if(isDelivery(route(i))){
        for(j <- segmentsArray.indices)
          if(segmentsArray(j) != null)
            if (segmentsArray(j)._1 != 0) {
              /**
                * If the segment doesn't contain the related pickup node it means that the related pickup node is before
                * the beginning of the segment and thus this is not possible to create a complete segment beginning
                * at this position.
                */
              if (!segmentsArray(j)._2.contains(getRelatedPickup(route(i))))
                segmentsArray(j) = null
              /**
                * Else we decrement the number of single pickup
                */
              else
                segmentsArray(j) = (segmentsArray(j)._1 - 1, segmentsArray(j)._2 :+ route(i))
            }
      }
    }

    var completeSegments: List[(Int, Int)] = Nil

    /**
      * We loop only on the segment that are not null and whose the number of single pickup is equals to 0
      */
    for(i <- segmentsArray.indices)if(segmentsArray(i) != null && segmentsArray(i)._1 == 0){
      val currentSegment = segmentsArray(i)._2
      completeSegments = (currentSegment.head, currentSegment.last) :: completeSegments
      var j = i-1
      var currentPreds = route(i-1)
      while(j != -1){
        if(segmentsArray(j) != null && currentPreds == segmentsArray(j)._2.last){
          completeSegments = (segmentsArray(j)._2.head, currentSegment.last) :: completeSegments
          currentPreds = route(j-1)
        }
        j -= 1
      }
    }
    completeSegments
  }


  // --------------------------------- Capacities -------------------------------------- //

  val vehicleMaxCapacity: Array[Int] = Array.tabulate(v)(_ => 0)

  /**
    * This array contains the content flow of each node of the problem.
    * At each node we can either load/unload article/passenger or do nothing.
    * If the value is positive => load, negative => unload, zero => do nothing.
    */
  val contentsFlow:Array[Int] = Array.tabulate(n)(_ => 0)

  def setVehicleMaxCapacities(maxCapacities: Array[Int]) = vehicleMaxCapacity.map(maxCapacities(_))

  /**
    * This method is used to set the content flow of each node except vehicle ones.
    * @param contents An array that contains the content flow of each node (except vehicle ones)
    */
  def defineContentsFlow(contents: Array[Int]): Unit ={
    require(contents.length == n-v,
      "Contents must have the size of the number of nodes (n-v)." +
        "\nn-v = " + (n-v) + ", contents's size : " + contents.length)
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

    for(i <- timeWindows.indices){

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

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    leaveTimes(node) <== (Max2(arrivalTimes(node), startWindow) + durationWithoutWait)
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
    * TODO : Transform it into a lambda method ?
    */
  def computeClosestNeighborInTime(filter : ((Int,Int) => Boolean) = (_,_) => true): Array[Iterable[Int]] ={
    Array.tabulate(n)(node => {
      val nodeCluster = arrivalTimeCluster.values(node).value
      val res:ListBuffer[Int] = new ListBuffer[Int]()
      //for(i <- 0 to Math.min(arrivalTimeCluster.clusters.length, nodeCluster))
      for(i <- 0 to nodeCluster)
        for(neighbor <- arrivalTimeCluster.clusters(i).value.toList if isRouted(neighbor) && filter(n,neighbor))
          if(leaveTimes(neighbor).value+travelDurationMatrix.getTravelDuration(neighbor, 0, node) < deadlines(node)) {
            val nextOfNeighbor = next(neighbor).value
            val neighborToNode = max(leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node), earlylines(node))
            val neighborToNodeToNext = neighborToNode + taskDurations(node) + travelDurationMatrix.getTravelDuration(node, 0, nextOfNeighbor)
            if(neighborToNodeToNext < deadlines(nextOfNeighbor))
              res.append(neighbor)
          }
      res.reverse.toList
    })
  }


/*
  //Draft for Jannou algo

  ForwardCumulativeIntegerDimensionOnVehicle(
    routes,
    n,
    v,
    (from,to,fromArrivalTime) => {
      val fromLeaveTime = fromArrivalTime + taskDuration(from)
      fromLeaveTime + Math.max(travelCosts.getTravelDuration(from, fromLeaveTime, to),earlylines(to))
    },
    Array.tabulate(v)(v => CBLSIntVar(m,0)),
    0,
    (node) => {
      if(maxWaitingTime(node) == Int.MaxValue)
        0
      else
        earlylines(node)-maxWaitingTime(node)
    },
    (node) => deadlines(node),
    contentName = "Arrival time"
  )*/
}
