package oscar.cbls.business.routing.model

import oscar.cbls.core.computation.{CBLSIntVar, IntValue, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.LE
import oscar.cbls.lib.invariant.routing.{ForwardCumulativeConstraintOnVehicle, ForwardCumulativeIntegerDimensionOnVehicle}

import scala.collection.immutable.List

/**
  * Created by fg on 28/04/17.
  */
class PDPv2 (override val n:Int, override val v:Int, override val m:Store, val d:Int, maxPivotPerValuePercent:Int = 4)
  extends VRP(n,v,m,maxPivotPerValuePercent) with NextAndPrev{

  val slowConstraints = new ConstraintSystem(m)

  /**
    * This list represents a list of drive.
    * Each drive is represented by an array of node (at least 2).
    * Each node represents a step in of the drive.
    * The first node is the pickup node and the last node the delivery node
    */
  val drives:Array[Array[Int]] = Array.tabulate(d)(_ => Array.empty)

  // The drive of each node
  val myDrive:Array[Array[Int]] = Array.tabulate(n-v)(_ => Array.empty)

  /**
    * This list represents the next step of each node (in his drive).
    */
  val nextStep:Array[Int] = Array.tabulate(n)(_ => 0)

  /**
    * This list represents the previous step of each node (in his drive).
    */
  val prevStep:Array[Int] = Array.tabulate(n)(_ => 0)

  /**
    * This array contains the content flow of each node of the problem.
    * At each node we can either load/unload article/passenger or do nothing.
    * If the value is positive => load, negative => unload, zero => do nothing.
    */
  val contentsFlow:Array[Int] = Array.tabulate(n)(_ => 0)


  /**
    * This method is used to set the drives of the problem.
    * @param drives The list of drives
    */
  def addDrives(drives: Array[Array[Int]]): Unit ={
    require(drives.length == d, "The drives length must have the same value as d (the number of drive). " +
      "\ndrives.length : " + drives.length + "    number of drives : " + d)
    for(i <- drives.indices) {
      val drive = drives(i)
      this.drives(i) = drive
      setPrevNext(drive)
    }
  }

  /**
    * This method is used to set the prevStep/nextStep value of each node contained in a drive.
    * The prevStep of the first node is himself and the nextStep of the last node is himself.
    * @param drive the drive
    */
  private def setPrevNext(drive: Array[Int]){
    for(i <- drive.indices){
      val node = drive(i)
      prevStep(node) = if(i == 0) node else drive(i-1)
      nextStep(node) = if(i == drive.length) node else drive(i+1)
      myDrive(node) = drive
    }
  }

  /**
    * This method is used to set the content flow of each node except vehicle ones.
    * @param contents An array that contains the content flow of each node (except vehicle ones)
    */
  def defineContentsFlow(contents: Array[Int]): Unit ={
    require(contents.length == n-v, "You must define the content variation for each node of each drive.")
    contentsFlow.map(n => contents(n))
  }

  /**
    * @return An array of unrouted Drives
    */
  def unroutedDrives={
    drives.filter(d => isRouted(d.apply(0)))
  }

  def pickupOfDrive(drive: Int) = drives(drive).head
  def isPickup(node: Int) = myDrive(node).head == node
  def getRelatedPickup(node: Int) = myDrive(node).head

  def deliveryOfDrive(drive: Int) = drives(drive).last
  def isDelivery(node: Int) = myDrive(node).last == node
  def getRelatedDelivery(node: Int) = myDrive(node).last




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
    val segmentsArray:Array[(Int,List[Int])] = new Array[(Int, List[Int])](d)

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


  // --------------------------------- Vehicles -------------------------------------- //

  val vehicleMaxCapacity: Array[Int] = Array.tabulate(v)(_ => 0)

  def setVehicleMaxCapacities(maxCapacities: Array[Int]) = vehicleMaxCapacity.map(maxCapacities(_))

  def addMaxCapacityConstraint() = {
    val vehiclesMaxCapacity:Int = vehicleMaxCapacity.sortBy(x => x).last
    val contentAtVehicleStart = Array.tabulate(v)(i => vehiclesMaxCapacity-vehicleMaxCapacity(i))
    ForwardCumulativeConstraintOnVehicle(
      routes,
      n,
      v,
      (from,to,fromContent) => fromContent + contentsFlow(to),
      vehiclesMaxCapacity,
      contentAtVehicleStart,
      4,
      4,
      "VehicleMaxCapacity"
    )
  }

  // --------------------------------- Time ---------------------------------------- //

  // The time at which we can start loading/unloading the vehicle
  val earlylines = Array.tabulate(n)(_ => 0)
  // The time before which we must have started loading/unloading the vehicle
  val deadlines = Array.tabulate(n)(_ => 0)
  // The duration of the task
  val taskDuration = Array.tabulate(n-v)(_ => 0)
  /*// The maxWaitingDuration at point
  val maxWaitingTime = Array.tabulate(n-v)(_ => 0)*/



  /*def addTimeWindowConstraint(travelCosts: TravelTimeFunction) = {
    val twConstraint = ForwardCumulativeIntegerDimensionOnVehicle(
      routes,
      n,
      v,
      (from,to,fromArrivalTime) => {
        val fromLeaveTime = fromArrivalTime + taskDuration(from)
        fromLeaveTime + Math.max(travelCosts.getTravelDuration(from, fromLeaveTime, to),earlylines(to))
      },
      Array.tabulate(v)(_ => CBLSIntVar(m,0)),
      0
    )

    for(i <- twConstraint._1.indices){
      val arrivalTime = twConstraint._1(i)
      slowConstraints.add(LE(arrivalTime+taskDuration(i),deadlines(i)))
    }
  }*/
}
