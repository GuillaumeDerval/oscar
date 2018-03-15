package oscar.examples.cbls.routing

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

/**
 * Created by rdl on 23/03/2015.
 */
object RoutingMatrixGenerator {
  val random = new Random(0)

  /**
    * This method generate a random distance matrix based on numbers of node and map side.
    * It also generate an array of node positions. (Usefull when you want to display it on a map)
    * @param N The number of nodes (considering depots)
    * @param side The side of the map
    * @return The distance matrix (Array[Array[Int] ] and the position of each node (Array[(Int,Int)])
    */
  def apply(N: Int, side: Int = 1000): (Array[Array[Int]],Array[(Int,Int)]) = {

    //we generate te cost distance matrix
    def randomXY: Int = (random.nextFloat() * side).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    def distance(from: (Int, Int), to: (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }


  /**
    * This method generate a random distance matrix based on numbers of node and map side.
    * It also generate an array of node positions. (Usefull when you want to display it on a map)
    * @param n The number of nodes (considering depots)
    * @return The distance matrix (Array[Array[Float] ] in meters and the position of each node (Array[(Double,Double)])
    */
  def geographicRandom(n: Int,minLong:Double,maxLong:Double,minLat:Double,maxLat:Double): (Array[Array[Double]],Array[(Double,Double)]) = {

    //we generate te cost distance matrix
    val deltaLong = maxLong - minLong
    val deltaLat = maxLat - minLat

    def randomLong: Double = (random.nextDouble() * deltaLong) + minLong
    def randomLat: Double = (random.nextDouble() * deltaLat) + minLat

    def distance(coord1:(Double,Double),coord2:(Double,Double)):Double = {
      val (lon1:Double,lat1:Double) = coord1
      val (lon2:Double,lat2:Double) = coord2

      val R = 6371e3 // metres

      val φ1 = lat1.toRadians
      val φ2 = lat2.toRadians
      val Δφ = (lat2 - lat1).toRadians
      val Δλ = (lon2 - lon1).toRadians

      val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
      val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

      (R * c).toDouble //meters
    }

    val pointPosition: Array[(Double,Double)] = Array.tabulate(n)(w => (randomLong, randomLat))

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(n)(
      n1 => Array.tabulate(n)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }


  /**
    * This method generate random restrictions for the problem.
    * A restriction is a tuple of a vehicle and a node.
    * This means that if the specified node is routed, it must be on the route of the specified vehicle
    * @param n The number of nodes (considering depots)
    * @param v The number of vehciles/depots
    * @param nbRestrictions The number of restrictions we want to generate
    * @return An iterable of tuple (node, vehicle)
    */
  def generateRestrictions(n:Int,v:Int,nbRestrictions:Int):Iterable[(Int,Int)] = {
    var toReturn = List.empty[(Int,Int)]

    var toGenerate = nbRestrictions
    while(toGenerate !=0){
      val vehicle = (random.nextFloat()*(v)).toInt
      val node = ((random.nextFloat()*(n-v))+v).toInt
      toReturn = (node,vehicle) :: toReturn
      toGenerate -= 1
    }
    toReturn
  }

  /**
    * Generate random precedences of size Max(2,random.nextInt(MaxSize))
    *
    * ex :  if length == 2 => (5,6)
    *       if length == 3 => (5,6,7) => (5,6) && (6,7)
    *       if length == 4 => (5,6,7,8) => (5,6) && (6,7) && (7,8)
    *
    * @param n The number of nodes (considering depots)
    * @param v The number of vehicles/depots
    * @param nbPRecedences The number wanted precedences
    * @param maxSize The max length of precedence you want to generate
    * @return A list of tuple (precedences)
    */
  def generateChainsPrecedence(n:Int, v:Int, nbPRecedences:Int, maxSize: Int = 2): (List[List[Int]], List[(Int,Int)]) = {
    val allNodes = v until n
    val randomizedNodes = random.shuffle(allNodes.toList).toIterator

    var currentMaxSize = maxSize
    var precedencesToGenerate = nbPRecedences

    var chains: List[List[Int]] = List.empty
    var tuples: List[(Int,Int)] = List.empty
    var usedNodes = 0

    def reduceCurrentSizeBy = Math.max(currentMaxSize - maxSize, (currentMaxSize-2)/2 - (usedNodes/precedencesToGenerate))
    def randomSize = Math.max(2,1 + random.nextInt(currentMaxSize))

    while(precedencesToGenerate > 0){
      currentMaxSize = currentMaxSize - reduceCurrentSizeBy
      precedencesToGenerate -= 1

      val chain: List[Int] = List.tabulate(randomSize)(_ => {
        usedNodes += 1
        randomizedNodes.next()
      })

      def toTuple(chain: List[Int], tuples: List[(Int,Int)]): List[(Int,Int)] ={
        chain match {
          case head :: Nil => tuples
          case head :: tail => toTuple(tail, (head, tail.head) :: tuples)
        }
      }
      chains = chain :: chains
      tuples = toTuple(chain, List.empty) ::: tuples
    }

    (chains,tuples)
  }

  /**
    * This method creates a List(node) for each node that doesn't belong to any chain.
    * @param n The number of node (considering depot)
    * @param chains The chains of the problem
    * @return An updated chains list (with lonely nodes)
    */
  private def addLonelyNodesToChains(n: Int, chains: List[List[Int]]): List[List[Int]]={
    val lonelyNodes = Range(0,n).toList.diff(chains.flatten)
    var precedencesWithLonelyNodes = chains
    for(lonelyNode <- lonelyNodes)
      lonelyNode :: precedencesWithLonelyNodes
    precedencesWithLonelyNodes
  }

  /**
    * This method generate a time window for each node such that the resolver can find a solution for the problem.
    *
    * It works like this :
    *   We add each precedence to a randomly selected vehicle.
    *     We randomly generate the earlyline, taskDuration and deadline for each node of the precedence
    *
    * This way there is always at least one solution for the problem.
    *
    * @param n The number of node of the problem (considering depot)
    * @param v The number of vehicle/depot
    * @param timeMatrix The travel time matrix
    * @param precedences The list of precedences
    * @param maxIdlingTimeInSec The maximum idling time (the time between two precedences)
    * @param maxExtraTravelTimeInSec The maximum extra travel time (time added to earlylines in order to have a more permissive time window)
    * @param maxTaskDurationInSec The maximum duration of a task
    * @return The time window for each node
    */
  def generateFeasibleTimeWindows(n:Int, v:Int,
                                  timeMatrix: TTFMatrix,
                                  precedences: List[List[Int]] = List.empty,
                                  maxIdlingTimeInSec: Int = 1800,
                                  maxExtraTravelTimeInSec: Int = 900,
                                  maxTaskDurationInSec: Int = 300): (Array[Int],Array[Int],Array[Int],Array[Int]) ={

    def randomVehicleSelection = random.nextInt(v)
    def randomIdleTime = random.nextInt(maxIdlingTimeInSec)
    def randomExtraTravelTime = random.nextInt(maxExtraTravelTimeInSec)
    def randomTaskDuration = random.nextInt(maxTaskDurationInSec)

    val precedencesWithLonelyNodes = addLonelyNodesToChains(n, precedences)
    val endOfLastActionOfVehicles = Array.fill(v)(0)
    val lastNodeVisitedOfVehicles = Array.tabulate(v)(x => x)
    val extraTravelTimeOfNodes = Array.tabulate(n)(node => if(node < v)0 else randomExtraTravelTime)
    val earlyLines = Array.fill(n)(0)
    val deadLines = Array.fill(n)(Int.MaxValue)
    val taskDurations = Array.tabulate(n)(node => if(node < v) 0 else randomTaskDuration)
    val maxWaitingDuration = Array.fill(n)(Int.MaxValue)

    /**
      * This method generate a time window for a given node on a given vehicle
      * It's divided in several step :
      *   1°  We add the travel time from the previous node to the current node to the endOfLastActionOfVehicles
      *   2°  We set the earlyline of the node
      *   3°  We add the taskDuration of the node to the endOfLastActionOfVehicles
      *   4°  We set the deadline of the node by adding a random extraTravelTime and
      *       the random extraTravelTime from the previous node to the endOfLastActionOfVehicles
      *   5°  We update the last node visited by the vehicle
      * @param node
      * @param onVehicle
      */
    def generateTimeWindowForNode(node: Int, onVehicle: Int): Unit ={
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

  /**
    * Based on precedences and earlylines, this methods generate a map of maxTravelDuration.
    * These values, when added to the constraints system ensure that the travel duration between A and B
    * doesn't exceeds a specified value.
    *
    * This generator set the maxTravelDuration's values like this :
    *
    * (travelDuration between A and B)*1.5
    *
    * e.g.: If the travel duration between A and B is 600, then the maxTravelDuration will be 900.
    * It means that the resolver can add a node (C) between A and B only if :
    *   leaveTime(A) + travel duration to C + taskDuration at C + travel duration to B <= 900
    *
    * @param precedences The list of precedences
    * @param earlylines The array of earlylines (We can't start the task at this node before the earlyline value)
    * @param travelDurationMatrix The travel time matrix
    * @return A Map[(from,to) -> maxTravelDuration]
    */
  def generateMaxTravelDurations(precedences: List[List[Int]],
                                 earlylines: Array[Int],
                                 travelDurationMatrix: TTFMatrix): Map[List[Int],Int] ={
    def maxTravelDurationOfPrecedence(from: Int, toProceed: List[Int], maxTravelDurations: List[(List[Int],Int)]): List[(List[Int],Int)] ={
      toProceed match{
        case Nil => maxTravelDurations
        case to::Nil => (List(from,to),(travelDurationMatrix.getTravelDuration(from,earlylines(from),to)*1.5).toInt) :: maxTravelDurations
        case to::tail => maxTravelDurationOfPrecedence(to, tail,(List(from,to),(travelDurationMatrix.getTravelDuration(from,earlylines(from),to)*1.5).toInt) :: maxTravelDurations)
      }
    }

    precedences.flatMap(p => maxTravelDurationOfPrecedence(p.head,p.tail,List.empty)).toMap
  }

  /**
    * Base on the distance matrix, this method generate a linear travel time matrix.
    *
    * e.g. :  859 distance units to go from A to B => 859 time units to go from A to B
    * @param n The nb of node in the problem (considering depots)
    * @param distanceMatrix The distance matrix (of size n*n)
    * @return A TTFMatrix object that represent the travel time matrix.
    */
  def generateLinearTravelTimeFunction(n:Int,distanceMatrix: Array[Array[Int]]): TTFMatrix = {
    val ttf = new TTFMatrix(n, new TTFConst(500))
    for (i <- 0 until n)
      for (j <- 0 until n)
        ttf.setTTF(i, j, new TTFConst(distanceMatrix(i)(j)))
    ttf
  }

  /**
    * This method return a random array of content flow.
    * Meaning when a vehicle arrives at a particular node, we add the value contentFlow(node) to the vehicle content.
    * This value is allways positive for heads of precedence (List[Int]) and always negative for last of precedence.
    * In between it's positive of negative. (random)
    * But the summed value of each precedence (Lisŧ[Int]) is equal to 0.
    *
    * By default, the content flow at depot is equal to zero.
    *
    * e.g.: +4, +2, -3, +5, -4, -4   or   +2, 0, 0, -2
    *
    * @param n The number of node of the problem
    * @param precedences The list of precedences (List[List[Int] ])
    * @param maxVehicleSize The max size of all vehicles in the problem (All the vehicles don't have to have the same size)
    * @return An array of Int that represents the vehicle content evolution when arriving at a given node.
    */
  def generateContentFlow(n: Int, precedences: List[List[Int]], maxVehicleSize: Int): Array[Int] ={
    def randomContent(currentMaxContent: Int) = random.nextInt(currentMaxContent)
    def isPickupStep: Boolean = random.nextBoolean()

    val contentFlow = Array.fill(n)(0)
    for(precedence <- precedences){
      var currentPrecedenceContent = 0
      for (node <- precedence){
        if(node == precedence.head) {
          contentFlow(node) = randomContent(maxVehicleSize)+1
          currentPrecedenceContent += contentFlow(node)
        } else if(node == precedence.last) {
          contentFlow(node) = -currentPrecedenceContent
        } else {
          contentFlow(node) =
            if(isPickupStep) randomContent(maxVehicleSize-currentPrecedenceContent)+1
            else -randomContent(currentPrecedenceContent)
          currentPrecedenceContent += contentFlow(node)
        }
      }
    }

    contentFlow
  }

  /**
    * This method generate a random array of vehicle size
    *
    * @param v the number of vehicle
    * @param maxVehicleSize the maximum vehicle size
    * @return an array of vehicle size
    */
  def generateVehiclesSize(v: Int, maxVehicleSize: Int, minVehicleSize: Int = 1): Array[Int] ={
    Array.fill(v)(Math.max(random.nextInt(maxVehicleSize)+1,minVehicleSize))
  }
}