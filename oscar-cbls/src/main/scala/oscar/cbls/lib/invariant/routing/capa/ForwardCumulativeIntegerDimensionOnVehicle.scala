package oscar.cbls.lib.invariant.routing.capa


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

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.{ErrorChecker, Checker}
import oscar.cbls.lib.invariant.routing.AbstractVehicleCapacity
import oscar.cbls.lib.invariant.routing.convention.VehicleLocation

/**
 * Created by  Jannou Brohée on 3/10/16.
 */

object ForwardCumulativeIntegerDimensionOnVehicle {
  /**
   * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,contentAtFromNode)=> contentAtToNode
   * @param contentAtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param defaultForUnroutedNodes is the content of a node that is not routed
   * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
   * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
   * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invairant
   * @return  (contentAtNode,contentAtEnd,lastPointOfVehicle)
   */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Int)=>Int,
            contentAtStart:Array[IntValue],
            defaultForUnroutedNodes:Int,
            minContent:Int = 0,
            maxContent:Int = Int.MaxValue,
            contentName:String = "content"):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar]) ={
    val contentAtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent).union(defaultForUnroutedNodes), contentName + " at node "+node))
    val contentAtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), contentName + " at end of route " + vehicle))
    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, n-1, "last point of vehicle" + vehicle))

    new ForwardCumulativeIntegerDimensionOnVehicle(routes,n,v,op,contentAtStart,contentAtNode,contentAtEnd,lastPointOfVehicle,defaultForUnroutedNodes)
    (contentAtNode,contentAtEnd,lastPointOfVehicle)
  }
}

class ForwardCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                 n:Int,
                                                 v:Int,
                                                 op:(Int,Int,Int)=>Int,
                                                 contentAtStart:Array[IntValue],
                                                 contentAtNode:Array[CBLSIntVar],
                                                 contentAtEnd:Array[CBLSIntVar],
                                                 lastPointOfVehicle:Array[CBLSIntVar],
                                                 defaultVehicleContentForUnroutedNodes:Int)

  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v) with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(contentAtStart)
  finishInitialization()
  for(i <- contentAtNode) i.setDefiningInvariant(this)
  for(i <- contentAtEnd) i.setDefiningInvariant(this)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int){
    toUpdateZonesAndVehicleStartAfter match {
      case None => ;
      case Some((toUpdateZones,vehicleLocation)) =>
        toUpdateZonesAndVehicleStartAfter = Some((toUpdateZones.insert(id, smartPrepend(0,0,toUpdateZones.getOrElse(id,List.empty[(Int,Int)]))),vehicleLocation))
    }
    scheduleForPropagation()
  }

  /**
   * @param vehicle
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtStart(vehicle : Int) : Boolean = {
    val newValue = contentAtStart(vehicle).value
    val oldValue = contentAtNode(vehicle).newValue
    if(oldValue == newValue){
      false
    }else{
      contentAtNode(vehicle) := newValue
      true
    }
  }

  /**
   * @param prevNode
   * @param node
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtNode(prevNode : Int, node : Int) : Boolean = {
    val oldValue = contentAtNode(node).newValue
    val newValue = op(prevNode,node,contentAtNode(prevNode).newValue)  //(fromNode,toNode,contentAtFromNode)=> contentAtToNode
    if(oldValue != newValue) {
      contentAtNode(node) := newValue
      true
    }else{
      false
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int){
    contentAtEnd(vehicle) := contentAtNode(lastNode).newValue
    lastPointOfVehicle(vehicle) := lastNode
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]){
    for(node <- unroutedNodes)
      contentAtNode(node) := defaultVehicleContentForUnroutedNodes
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c,routes.value)
  }
  def check(c : Checker,s:IntSequence){
    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) =
      AbstractVehicleCapacity.computeNodeToIntContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(n,v,op,v => contentAtStart(v).value,s,defaultVehicleContentForUnroutedNodes)
    val currentVehicleLocation = this.toUpdateZonesAndVehicleStartAfter.get._2

    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == s.positionOfAnyOccurrence(vehicle).get,
        Some("Found start of vehicle(" + vehicle + "):=" + vehicleLocation.startPosOfVehicle(vehicle) + " should be :=" + s.positionOfAnyOccurrence(vehicle) +" seq :"+ s.mkString(",")))
      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }

    for(node <- 0 until n){
      c.check(nodeToContent(node) == contentAtNode(node).newValue,
        Some("Vehicle content at node(" + node + ") at pos : "+ s.positionsOfValue(node)+ " := " + contentAtNode(node).newValue + " should be :=" + nodeToContent(node)+ " routes:" + s.mkString(",")  + " contentAtStart:" + contentAtStart.mkString(",")))
    }

    for(vehicle <- 0 until v){
      c.check(contentAtEnd(vehicle).newValue == vehicleToContentAtEnd(vehicle), Some("Error on vehicle content at end vehicle:" + vehicle + " contentAtEnd(vehicle).newValue:" + contentAtEnd(vehicle).newValue + " should be:" +  vehicleToContentAtEnd(vehicle)))
    }
  }
}

