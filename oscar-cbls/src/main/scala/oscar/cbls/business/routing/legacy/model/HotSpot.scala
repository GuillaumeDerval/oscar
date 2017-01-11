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

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.legacy.neighborhood.VRPMove
import oscar.cbls.core.search.{CompositeMove, Move}

import scala.collection.immutable.SortedSet
import scala.collection.mutable.Queue

trait HotSpot extends VRP with RoutedAndUnrouted with PositionInRouteAndRouteNr{

  var lastHotSpotStep:HotSpotHistoryStep = new HotSpotHistoryStep()

  def updateHotSpotAfterMoveAnyMove(m:Move): Unit ={
    m match{
      case i:VRPMove with HotSpottingInfo =>
        updateHotSpotAfterMove(i)
      case CompositeMove(moves, _, _) =>
        moves.foreach(updateHotSpotAfterMoveAnyMove)
      case _ => makeEverythingHot
    }
  }

  def widenImpactedPoints(node:Int, acc:List[Int]):List[Int] = {
    node::next(node).newValue::acc
  }

  def updateHotSpotAfterMove(m: VRPMove with HotSpottingInfo){
    val impacted = m.impactedPoints
    val (unrouted,impactedVehicles,impactedPoints) = impacted.foldLeft((List.empty[Int],List.empty[Int],List.empty[Int]))((acc,node) => {
      val routeOfPoint = routeNr(node).value
      if(routeOfPoint == V) (node :: acc._1,acc._2,acc._3)         //unrouted
      else (acc._1,routeOfPoint :: acc._2, widenImpactedPoints(node,acc._3))        //routed, vehicle V
    })

    val newStep = new HotSpotHistoryStep(impactedVehicles,impactedPoints,unrouted)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def makeEverythingHot() {
    val newStep = new HotSpotHistoryStep(this.vehicles.toList,this.routed.value.toList,this.unrouted.value.toList)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def newVehicleHotSpotView(hotRestart:Boolean) = new HotSpotViewPerVehicle(this,hotRestart)

  def newUnroutedHotSpotView(hotRestart:Boolean) = new HotSpotViewOfUnroutedNodes(this, hotRestart)

  def newNodeHotSpotView(hotRestart:Boolean) = new HotSpotViewOfRoutedNodes(this, hotRestart)
}

trait HotSpottingInfo{
  def impactedPoints:Iterable[Int]
}

case class HotSpotHistoryStep(updatedVehicles:List[Int]=Nil,
                              impactedPoints:List[Int]=Nil,
                              unroutedPoints:List[Int]=Nil,
                              var nextStep:HotSpotHistoryStep = null)

class HotSpotViewPerVehicle(vrp:VRP with HotSpot, hotRestart:Boolean){

  var lastIteratedPoint:Int = -1

  var hotVehiclesSet:SortedSet[Int] = SortedSet.empty[Int] ++ vrp.vehicles
  val hotVehicleArray:Array[Boolean] = Array.fill(vrp.V)(true)
  val hotVehicleQueue:Queue[Int] = new Queue[Int] ++ vrp.vehicles

  var lastDigestedUpdate = vrp.lastHotSpotStep

  def setFirstVehiceCold(): Unit ={
    val vehicle = hotVehicleQueue.dequeue()
    hotVehiclesSet -= vehicle
    hotVehicleArray(vehicle) = false
    //    println("after setting first vehicle cold:" + hotVehicleQueue)
  }

  def setVehicleHot(v:Int): Unit ={
    if(!hotVehicleArray(v)){
      hotVehicleQueue.enqueue(v)
      hotVehiclesSet += v
      hotVehicleArray(v) = true
    }
  }

  def hotNodesByVehicleWithConsumption:Iterable[Int] =
    new HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp, this, lastIteratedPoint)

  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      lastDigestedUpdate.updatedVehicles.foreach(setVehicleHot)
    }
    if(hotRestart){
      if(hotVehicleQueue.nonEmpty) hotVehicleQueue.enqueue(hotVehicleQueue.dequeue())
    }
  }

  class HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp:VRP,origin:HotSpotViewPerVehicle,lastIteratedPoint:Int) extends Iterable[Int]{
    override def iterator: Iterator[Int] = {
      origin.updateFromHotSpot
      new RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotViewPerVehicle,lastIteratedPoint)
    }
  }


  class RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotViewPerVehicle,lastIteratedPoint:Int) extends Iterator[Int]{
    //HotRestart is implemented by rolling the vehicle at each update of the HotSpot
    //TODO: we should improve on this by staying on the last vehicle, and performing a hot restart on the position in this vehicle. in order to set it cold as quickly as possible.

    /*  var remainingPointsOfVehicle:List[Int] =
        if(origin.hotVehicleQueue.isEmpty) Nil
        else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head)*/


    var pointsOfVehicle:Iterator[Int] =
      HotRestart.hotRestartPreserveSequence((if(origin.hotVehicleQueue.isEmpty) Nil
      else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head)),lastIteratedPoint).iterator

    //  println("YYYYYYYYYYYYYYYYYYYYYYYY RoutedNodeIteratorWithConsumption ")
    //  println("origin.hotVehicleQueue" + origin.hotVehicleQueue)

    override def hasNext: Boolean =
      pointsOfVehicle.hasNext || origin.hotVehicleQueue.nonEmpty

    override def next(): Int = {

      if(!pointsOfVehicle.hasNext) {
        pointsOfVehicle =
          (if (origin.hotVehicleQueue.isEmpty) throw new Error("no next available, actually")
          else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head).iterator)

        return this.next()
      }

      val next = pointsOfVehicle.next()

      if(!pointsOfVehicle.hasNext) {
        origin.setFirstVehiceCold()
      }
      origin.lastIteratedPoint = next
      next
    }
  }
}

class HotSpotViewOfUnroutedNodes(vrp:VRP with HotSpot, hotRestart:Boolean)
extends HotSpotViewOfNodes(vrp, hotRestart) {
  override def digestHotSpotStep(hotSpotHistoryStep: HotSpotHistoryStep){
    hotSpotHistoryStep.unroutedPoints.foreach(setNodeHot)
  }
}


class HotSpotViewOfRoutedNodes(vrp:VRP with HotSpot, hotRestart:Boolean)
  extends HotSpotViewOfNodes(vrp, hotRestart) {
  override def digestHotSpotStep(hotSpotHistoryStep: HotSpotHistoryStep){
    hotSpotHistoryStep.impactedPoints.foreach(setNodeHot)
  }
}


abstract class HotSpotViewOfNodes(vrp:VRP with HotSpot, hotRestart:Boolean){
  var hotPointsArray:Array[Boolean] = Array.tabulate(vrp.N)(_ => false)
  val hotPointsQueue:Queue[Int] = new Queue[Int]
  var lastDigestedUpdate = vrp.lastHotSpotStep

  def setNodeCold(node:Int){
    require(node == hotPointsQueue.head)
    hotPointsQueue.dequeue()
    hotPointsArray(node) =  false
  }

  def setNodeHot(node:Int): Unit ={
    if(! hotPointsArray(node)){
      hotPointsQueue.enqueue(node)
      hotPointsArray(node) = true
    }
  }

  def digestHotSpotStep(hotSpotHistoryStep: HotSpotHistoryStep)

  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      digestHotSpotStep(lastDigestedUpdate)
    }
    if(hotRestart){
      if (hotPointsQueue.nonEmpty) hotPointsQueue.enqueue(hotPointsQueue.dequeue())
    }
  }

  /**
   * before calling this, ensure that updateFromHotSpot was called since last update.
   * @param node
   * @return
   */
  def isNodeHot(node:Int):Boolean = hotPointsArray(node)

  def hotNodesWithConsumption:Iterable[Int] =
    new HotSpotIterableOnNodesWithConsumption(vrp, this)

  class HotSpotIterableOnNodesWithConsumption(vrp:VRP,origin:HotSpotViewOfNodes) extends Iterable[Int]{
    override def iterator: Iterator[Int] = {
      origin.updateFromHotSpot
      new UnroutedIteratorWithConsumption(vrp, origin)
    }
  }

  class UnroutedIteratorWithConsumption(vrp:VRP,origin:HotSpotViewOfNodes) extends Iterator[Int]{

   // println("HotSpotSize: " + hotPointsQueue.size)
    //TODO: some post-filtering here!
    override def hasNext: Boolean = hotPointsQueue.nonEmpty

    override def next(): Int = {
      val toReturn = hotPointsQueue.head
      origin.setNodeCold(toReturn)
      toReturn
    }
  }
}


