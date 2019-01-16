package oscar.cbls.benchmarks.vrp

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


import oscar.cbls.business.routing.model.{TTFConst, TTFMatrix}

import scala.util.Random

object RoutingMatrixGenerator {
  val random = new Random(0L)

  def apply(n: Int, side: Long = 1000L): (Array[Array[Long]],Array[(Long,Long)]) = {

    //we generate te cost distance matrix
    def randomXY: Long = (random.nextFloat() * side).toInt
    val pointPosition: Array[(Long, Long)] = Array.tabulate(n)(_ => (randomXY, randomXY))

    def distance(from: (Long, Long), to: (Long, Long)) =
      math.sqrt(math.pow(from._1 - to._1, 2L) + math.pow(from._2 - to._2, 2L)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(n)(
      n1 => Array.tabulate(n)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }

  def generateRestrictions(n:Int,v:Int,nbRestrictions:Int):Iterable[(Int,Int)] = {
    var toReturn = List.empty[(Int,Int)]

    var toGenerate = nbRestrictions
    while(toGenerate !=0){
      val vehicle = (random.nextFloat()*v).toInt
      val node = ((random.nextFloat()*(n-v))+v).toInt
      toReturn = (node,vehicle) :: toReturn
      toGenerate -= 1
    }
    toReturn
  }

  def generatePrecedence(n:Long,v:Long,nbPRecedences:Long):List[(Long,Long)] = {
    val allNodes = v until n

    val randomizedNodes = random.shuffle(allNodes.toList).toIterator
    var precedencesToGenerate = nbPRecedences
    var toReturn:List[(Long,Long)] = List.empty

    while(precedencesToGenerate > 0L){
      precedencesToGenerate -= 1L
      toReturn = (randomizedNodes.next(),randomizedNodes.next()) :: toReturn
    }

    toReturn
  }

  def addLonelyNodesToPrecedences(n: Int, precedences: List[List[Int]]): List[List[Int]]={
    val lonelyNodes = Range(0,n).toList.diff(precedences.flatten)
    var precedencesWithLonelyNodes = precedences
    for(lonelyNode <- lonelyNodes)
      lonelyNode :: precedencesWithLonelyNodes
    precedencesWithLonelyNodes
  }

  def generateFeasibleTimeWindows(n:Int, v:Int,
                                  timeMatrix: TTFMatrix,
                                  precedences: List[List[Int]] = List.empty,
                                  maxIdlingTimeInSec: Long = 1800L,
                                  maxExtraTravelTimeInSec: Long = 900L,
                                  maxTaskDurationInSec: Long = 300L): (Array[Long],Array[Long],Array[Long],Array[Long]) ={

    def randomVehicleSelection = random.nextInt(v)
    def randomIdleTime = random.nextLong(maxIdlingTimeInSec)
    def randomExtraTravelTime = random.nextLong(maxExtraTravelTimeInSec)
    def randomTaskDuration = random.nextLong(maxTaskDurationInSec)

    val precedencesWithLonelyNodes = addLonelyNodesToPrecedences(n, precedences)
    val endOfLastActionOfVehicles = Array.fill(v)(0L)
    val lastNodeVisitedOfVehicles = Array.tabulate(v)(x => x)
    val extraTravelTimeOfNodes = Array.tabulate(n)(node => if(node < v)0L else randomExtraTravelTime)
    val earlyLines = Array.fill(n)(0L)
    val deadLines = Array.fill(n)(Long.MaxValue)
    val taskDurations = Array.tabulate(n)(node => if(node < v) 0L else randomTaskDuration)
    val maxWaitingDuration = Array.fill(n)(Long.MaxValue)

    /**
      * This method generate a time window for a given node on a given vehicle
      * It's divided in several step :
      *   1L°  We add the travel time from the previous node to the current node to the endOfLastActionOfVehicles
      *   2L°  We set the earliestArrivalTime of the node
      *   3L°  We add the taskDuration of the node to the endOfLastActionOfVehicles
      *   4L°  We set the latestLeavingTime of the node by adding a random extraTravelTime and
      *       the random extraTravelTime from the previous node to the endOfLastActionOfVehicles
      *   5L°  We update the last node visited by the vehicle
      * @param node
      * @param onVehicle
      */
    def generateTimeWindowForNode(node: Long, onVehicle: Long): Unit ={
      val previousNode = lastNodeVisitedOfVehicles(onVehicle)
      val travelFromPreviousNode = timeMatrix.getTravelDuration(previousNode,endOfLastActionOfVehicles(onVehicle),node)
      endOfLastActionOfVehicles(onVehicle) += travelFromPreviousNode

      earlyLines(node) = endOfLastActionOfVehicles(onVehicle)
      endOfLastActionOfVehicles(onVehicle) += taskDurations(node)
      deadLines(node) = endOfLastActionOfVehicles(onVehicle) + extraTravelTimeOfNodes(node) + extraTravelTimeOfNodes(previousNode)
      lastNodeVisitedOfVehicles(onVehicle) = node
    }

    for(precedence <- precedencesWithLonelyNodes){
      val vehicle = randomVehicleSelection
      //Add some idle time
      endOfLastActionOfVehicles(vehicle) += randomIdleTime
      for(node <- precedence){
        generateTimeWindowForNode(node,vehicle)
      }
    }
    (earlyLines,deadLines,taskDurations,maxWaitingDuration)
  }

  def generateLinearTravelTimeFunction(n:Long,distanceMatrix: Array[Array[Long]]): TTFMatrix = {
    val ttf = new TTFMatrix(n, new TTFConst(500L))
    for (i <- 0L until n)
      for (j <- 0L until n)
        ttf.setTTF(i, j, new TTFConst(distanceMatrix(i)(j)))
    ttf
  }
}
