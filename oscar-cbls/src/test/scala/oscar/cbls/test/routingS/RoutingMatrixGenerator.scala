package oscar.cbls.test.routingS

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


import oscar.cbls.routing.seq.model.{TTFConst, TTFMatrix, PDP}

import scala.util.Random

object RoutingMatrixGenerator {
  val random = new Random(0)

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

  def generatePrecedence(n:Int,v:Int,nbPRecedences:Int):List[(Int,Int)] = {
    val allNodes = v until n

    val randomizedNodes = random.shuffle(allNodes.toList).toIterator
    var precedencesToGenerate = nbPRecedences
    var toReturn:List[(Int,Int)] = List.empty

    while(precedencesToGenerate > 0){
      precedencesToGenerate -= 1
      toReturn = (randomizedNodes.next(),randomizedNodes.next()) :: toReturn
    }

    toReturn
  }

  def generateTimeWindows(n:Int, v:Int, pickups:Array[Int], deliveries:Array[Int], timeUnitDelta:Int = 50): Array[(Int,Int,Int,Int)] ={

    val maxNodes = n-v
    val maxCouples = maxNodes/2
    val random = new Random(0)
    val timeWindows = Array.tabulate(n-v)(tw => (0,0,0,0))
    var tWCoupleGenerated = 0
    var currentTimeUnit = 0


    while(tWCoupleGenerated < maxCouples){
      val nbOfCouplesToAdd = Math.min(maxCouples - tWCoupleGenerated,random.nextInt(v))
      //println(nbOfCouplesToAdd)
      for(inc <- 0 until nbOfCouplesToAdd){
        val incDelivery = random.nextInt(50)
        timeWindows(pickups(tWCoupleGenerated+inc)-v) = (timeUnitDelta*(currentTimeUnit+1),Int.MaxValue,timeUnitDelta,0)
        timeWindows(deliveries(tWCoupleGenerated+inc)-v) = (-1,timeUnitDelta*(currentTimeUnit+1+incDelivery),timeUnitDelta,0)
      }
      tWCoupleGenerated += nbOfCouplesToAdd
      currentTimeUnit += random.nextInt(10*1000/timeUnitDelta)
    }
    timeWindows
  }

  def generatePickupDeliveryCouples(n:Int, v:Int): (Array[Int],Array[Int]) ={
    require((n-v)%2 == 0,"You must have a pair number of nodes")
    (Array.tabulate((n-v)/2)(p => p+v), Array.tabulate((n-v)/2)(d => d+v+((n-v)/2)))
  }

  def generateLinearTravelTimeFunction(n:Int,distanceMatrix: Array[Array[Int]]): TTFMatrix = {
    val ttf = new TTFMatrix(n, new TTFConst(500))
    for (i <- 0 until n)
      for (j <- 0 until n)
        ttf.setTTF(i, j, new TTFConst(distanceMatrix(i)(j)))
    ttf
  }
}


