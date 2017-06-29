package oscar.cbls.business.routing.legacy.model

/*******************************************************************************
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
  ******************************************************************************/

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.IntVarObjective
import oscar.cbls.lib.constraint.{EQ, GE, LE}
import oscar.cbls.lib.invariant.logic.{Cluster, DenseCluster, IntITE, IntInt2Int}
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.numeric.Div
import oscar.cbls.lib.invariant.routing.VehicleOfNodes
import oscar.cbls.lib.invariant.seq.Precedence
import oscar.cbls.modeling.Algebra._

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.math._

/**
  * Created by fabian on 04-07-16.
  */
class PDP(override val n:Int, override val v:Int, override val m:Store, maxPivotPerValuePercent:Int = 4)
  extends oscar.cbls.business.routing.model.VRP(n,v,m,maxPivotPerValuePercent) with oscar.cbls.business.routing.model.NextAndPrev{

  /**
    * The array that maintains pickup and delivery node.
    * Each value (representing a node) contains the load value of the node
    * (negative value for a delivery node, positive for a pickup node and 0 for a depot)
    * and the value of the related node.
    */
  val pickupDeliveryNodes:Array[(Int,Int)] = new Array[(Int, Int)](n)
  for(i <- 0 until v)pickupDeliveryNodes(i) = (0,i)

  val pickupNodes:Array[Int] = new Array[Int]((n-v)/2)
  val deliveryNodes:Array[Int] = new Array[Int]((n-v)/2)

  val vehicleOfNodes = VehicleOfNodes(routes,v)

  val fastConstraints = new ConstraintSystem(m)
  val slowConstraints = new ConstraintSystem(m)
  var precedenceObj:IntVarObjective = null


  /**
    * Add a new PickupAndDelivery couple and set this two basic constraints :
    * A strong constraint that specified that each pickup node has to be before his related delivery node
    * A strong constraint that specified that each couple of point (pickup and delivery) has to be in the same route.
    *
    * @param p the pickup point
    * @param d the delivery point
    */
  def addPickupDeliveryCouple(p:Int, d:Int, coupleNumber:Int, value:Int = 1): Unit ={
    pickupDeliveryNodes(p) = (value,d)
    pickupDeliveryNodes(d) = (-value,p)
    pickupNodes(coupleNumber) = p
    deliveryNodes(coupleNumber) = d
    fastConstraints.post(EQ(vehicleOfNodes(p),vehicleOfNodes(d)))
  }

  /**
    * Add defined couples.
    * The user has to specify two arrays. One containing the pickup points and
    * another one containing the related delivery points. So pickup(1) is the pickup point of delivery(1).
    *
    * @param pickups the pickup points array
    * @param deliveries the delivery points array
    * @param nbPassengers the number of passengers for the ride (leave empty if you want to use the one passenger per ride scenario)
    */
  def addPickupDeliveryCouples(pickups:Array[Int], deliveries:Array[Int], nbPassengers:Array[Int] = Array.tabulate((n-v)/2)(p => 1)): Unit ={
    assert(pickups.length == deliveries.length,
      "The pickup array and the delivery array must have the same length.")
    assert(!pickups.exists(_ >= n) || !deliveries.exists(_ >= n),
      "The pickup and the delivery array may only contain values between 0 and the number of nodes")
    assert(pickups.intersect(deliveries).length == 0,
      "One node can't be a pickup node and a delivery node at the same time")

    val precedenceList = List.tabulate((n-v)/2)(c => (pickups(c),deliveries(c)))
    precedenceObj = new IntVarObjective(Precedence(routes,precedenceList))
    for(i <- pickups.indices){
      addPickupDeliveryCouple(pickups(i),deliveries(i),i,nbPassengers(i))
    }
  }

  /**
    * This method check if the given node is a pickup one. (if his load value is > 0 it's a pickup node)
    *
    * @param index the index of the node
    * @return
    */
  def isPickup(index:Int): Boolean = pickupDeliveryNodes(index)._1 > 0

  def isUnroutedPickup(index:Int): Boolean = isPickup(index) && isUnrouted(index)

  /**
    * This method check if the given node is a delivery one. (if his load value is < 0 it's a delivery node)
    *
    * @param index the index of the node
    * @return
    */
  def isDelivery(index:Int):Boolean = pickupDeliveryNodes(index)._1 < 0

  def getRelatedPickup(d:Int): Int ={
    require(pickupDeliveryNodes(d)._1 < 0,"This node isn't a delivery node")
    pickupDeliveryNodes(d)._2
  }

  def getRelatedDelivery(p:Int): Int ={
    require(pickupDeliveryNodes(p)._1 > 0,"This node isn't a pickup node")
    pickupDeliveryNodes(p)._2
  }

  def getRelatedNode(node:Int): Int ={
    require(node >= v, "You must specify a node, not a vehicle")
    pickupDeliveryNodes(node)._2
  }

  /**
    * @param index the index of a node
    * @return the load value of the node
    */
  def loadValueAtNode(index:Int): Int ={
    pickupDeliveryNodes(index)._1
  }

  def getPickups: Iterable[Int] = pickupNodes

  def getDeliverys: Iterable[Int] = deliveryNodes

  def getRoutedPickups: Iterable[Int] = pickupNodes.filter(isRouted(_))

  def getRoutedDeliverys: Iterable[Int] = deliveryNodes.filter(isRouted(_))

  def getUnroutedPickups: Iterable[Int] = pickupNodes.filter(!isRouted(_))

  def getUnroutedDeliverys: Iterable[Int] = deliveryNodes.filter(!isRouted(_))

  def getNodesBeforeRelatedDelivery()(node:Int):Iterable[Int] = {
    assert(isPickup(node), "The referenced node must be a pickup one !")
    getNodesBeforePosition()(getRelatedDelivery(node))
  }

  def getNodesAfterRelatedPickup()(node: Int): Iterable[Int] = {
    assert(isDelivery(node), "The referenced node must be a delivery one !")
    getNodesAfterNode()(getRelatedPickup(node))
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
    val segmentsArray:Array[(Int,List[Int])] = new Array[(Int, List[Int])](route.size)

    for(i <- route.indices){
      if(isPickup(route(i))){
        //If the node is a pickup one, we add the node to all the active segment and we create a new one
        for(j <- segmentsArray.indices)
          if(segmentsArray(j) != null)
            if (segmentsArray(j)._1 != 0)
              segmentsArray(j) = (segmentsArray(j)._1 + 1, segmentsArray(j)._2 :+ route(i))
        segmentsArray(i) = (1,route(i)::Nil)
      }
      else{
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


  def getUnCompleteSegments(routeNumber:Int): List[List[Int]] ={
    var unCompleteSegmentsList:List[List[Int]] = Nil
    var currentList:List[Int] = Nil
    val route = getRouteOfVehicle(routeNumber)
    for(node <- route){
      if(isPickup(node))
        currentList = node :: currentList
      else if(isDelivery(node)){
        if(!currentList.contains(getRelatedPickup(node)))
          currentList = node :: currentList
        else{
          val pos = currentList.indexOf(getRelatedPickup(node))
          unCompleteSegmentsList = currentList.reverse :: unCompleteSegmentsList
          currentList = node :: currentList.dropRight(currentList.size-pos)
        }
      }
    }
    currentList.reverse :: unCompleteSegmentsList
  }


  //---- Vehicle capacity ----//

  val defaultArrivalLoadValue = 0

  //We create a array of size v+1 so when we initiate constraints the max capacity for unrouted node is null.
  //Maybe not the best solution but it works.
  val vehicleMaxCapacity:Array[Int] = Array.tabulate(v+1) ((vehicle:Int) => 0)

  val arrivalLoadValue = Array.tabulate(n){ (i:Int) => CBLSIntVar(m, 0, FullRange, "Arrival load at node " + i)}

  var leaveLoadValue:Array[IntValue] = null

  def setArrivalLeaveLoadValue(): Unit ={
    leaveLoadValue = Array.tabulate(n+1) {
      (i :Int) =>
        if(i == n || i < v)
          defaultArrivalLoadValue
        else
          arrivalLoadValue(i) + loadValueAtNode(i)
    }
    for(i <- 0 until n){
      arrivalLoadValue(i) <== leaveLoadValue.element(prev(i))
    }
  }

  def setVehiclesMaxCargo(max:Int): Unit ={
    for(i <- 0 until v)
      vehicleMaxCapacity(i) = max
  }

  def setVehiclesCapacityStrongConstraint(): Unit ={
    for(i <- arrivalLoadValue.indices)
      slowConstraints.post(LE(arrivalLoadValue(i), vehicleMaxCapacity.element(vehicleOfNodes(i))))
  }

  def isNotFull()(node:Int): Boolean ={
    arrivalLoadValue(node).value < 5
  }

  //---- Time Windows ----//

  var defaultArrivalTime:CBLSIntConst = _

  var arrivalTimes:Array[CBLSIntVar] = Array.empty

  var leaveTimes:Array[CBLSIntVar] = Array.empty

  var travelOutDurations:Array[CBLSIntVar] = Array.empty

  var arrivalTimesToNext:Array[IntValue] = Array.empty

  var travelDurationMatrix: TravelTimeFunction = _

  var timeWindows: Array[(Int,Int,Int,Int)] = Array.empty

  var arrivalTimeCluster: DenseCluster[IntValue] = _

  var waitingDurations: Array[IntValue] = Array.empty

  def initiateTimeWindowInvariants(): Unit ={
    defaultArrivalTime = new CBLSIntConst(0)

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

    arrivalTimeCluster = Cluster.MakeDenseAssumingMinMax(leaveTimes.map(x => Div(x,900)),0,192)
  }

  //Leave before ...
  def setEndWindow(node: Int, endWindow: Int) {
    require(node >= v, "only for specifying time windows on nodes, not on vehicles")
    slowConstraints.post(LE(IntITE(next(node), 0, leaveTimes(node), n-1), endWindow).nameConstraint("end of time window on node " + node))
  }

  def setVehicleEnd(vehicle: Int, endTime: Int) {
    require(vehicle < v, "only for specifying end time of vehicles")
    slowConstraints.post(LE(arrivalTimes(vehicle), endTime).nameConstraint("end of time for vehicle " + vehicle))
  }

  def setNodeDuration(node: Int, duration: IntValue) {
    assert(node >= v)
    leaveTimes(node) <== (arrivalTimes(node) + duration)
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int) {
    leaveTimes(node) <== (Max2(arrivalTimes(node), startWindow) + durationWithoutWait)
  }

  def setNodeDuration(node: Int, durationWithoutWait: IntValue, startWindow: Int, maxWaiting: Int) {
    setNodeDuration(node, durationWithoutWait, startWindow)
    slowConstraints.post(GE(arrivalTimes(node), startWindow - maxWaiting).nameConstraint("end of time window on node (with duration)" + node))
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

  /**
    * This method initialize the differents variable used to represent time windows
    * @param timeWindows contains an array of the time to attribute.
    *                    Each value of this array represents these values :
    *                    - The time after which we may arrive to this point (Default value = -1)
    *                    - The time before which we must have reach and performed the task of this point (Default value = Int.MaxValue)
    *                    - The execution's duration of the task related to this point (Default value = 0)
    *                    - The max waiting time of this point (Default value = 0)
    */
  def setTimeWindows(timeWindows : Array[(Int,Int,Int,Int)]): Unit ={
    initiateTimeWindowInvariants()
    val tWS:Array[(Int,Int,Int,Int)] = new Array[(Int,Int,Int,Int)](n)

    for(k <- 0 until v)
      tWS(k) = (-1,Int.MaxValue,0,0)

    for(i <- timeWindows.indices){
      (timeWindows(i)._1, timeWindows(i)._4) match {
        case (-1,0) => setNodeDuration(i + v, timeWindows(i)._3)
        case (_,0) => setNodeDuration(i + v, timeWindows(i)._3, timeWindows(i)._1)
        case (_,_) => setNodeDuration(i + v, timeWindows(i)._3, timeWindows(i)._1, timeWindows(i)._4)
      }
      if(timeWindows(i)._2 < Int.MaxValue)
        setEndWindow(i + v, timeWindows(i)._2)
      tWS(i+v) = timeWindows(i)
    }
    this.timeWindows = tWS


    waitingDurations = Array.tabulate(n){
      (i:Int) =>
        if(i >= v)
          Max2(leaveTimes(i) - timeWindows(i-v)._3 - arrivalTimes(i), CBLSIntConst(0))
        else
          new CBLSIntConst(0)
    }
  }

  def setUniqueMaxTravelDistance(value:Double, multiple:Boolean = false){
    for(p <- getPickups) {
      val d = getRelatedDelivery(p)

      //TODO: on ne peu pas appeler leaveTime(p).value; c'est hors du moteur d'optim.
      //à prioris, on cherche plutôt la durée du trajet à l'heure de pick-up demandée
      if(multiple)
        slowConstraints.post(LE(arrivalTimes(d) - leaveTimes(p), (value*travelDurationMatrix.getTravelDuration(p, 0, d)).toInt))
      else
        slowConstraints.post(LE(arrivalTimes(d) - leaveTimes(p), (value+travelDurationMatrix.getTravelDuration(p, 0, d)).toInt))
    }
  }

  def setMultipleMaxTravelDistance(values:Array[Double], multiple:Boolean = false): Unit ={
    require(values.length == (n-v)/2, "The size of values must be equals to the number of couple pickup/delivery")
    for(i <- values.indices){
      val p = pickupNodes(i)
      val d = getRelatedDelivery(p)
      if(multiple)
        slowConstraints.post(LE(arrivalTimes(d) - leaveTimes(p), (values(i)*travelDurationMatrix.getTravelDuration(p, leaveTimes(p).value, d)).toInt))
      else
        slowConstraints.post(LE(arrivalTimes(d) - leaveTimes(p), (values(i)+travelDurationMatrix.getTravelDuration(p, leaveTimes(p).value, d)).toInt))
    }
  }

  /**
    * This method compute the closest neighbor of a node base on arrivalTime.
    *
    */
  def computeClosestNeighborInTime(filter : ((Int,Int) => Boolean) = (_,_) => true): Array[Iterable[Int]] ={
    Array.tabulate(n)(node => {
      val nodeCluster = arrivalTimeCluster.values(node).value
      val res:ListBuffer[Int] = new ListBuffer[Int]()
      for(i <- 0 to Math.min(arrivalTimeCluster.clusters.length, nodeCluster))
        for(neighbor <- arrivalTimeCluster.clusters(i).value.toList if isRouted(neighbor) && filter(n,neighbor))
          if(leaveTimes(neighbor).value+travelDurationMatrix.getTravelDuration(neighbor, 0, node) < timeWindows(node)._2) {
            val nextOfNeighbor = next(neighbor).value
            val neighborToNode = max(leaveTimes(neighbor).value + travelDurationMatrix.getTravelDuration(neighbor, 0, node), timeWindows(node)._1)
            val neighborToNodeToNext = neighborToNode + timeWindows(node)._3 + travelDurationMatrix.getTravelDuration(node, 0, nextOfNeighbor)
            if(neighborToNodeToNext < timeWindows(nextOfNeighbor)._2)
              res.append(neighbor)
          }
      res.reverse.toList
    })
  }

}