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

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.computation.ChangingIntValue
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.core.computation.Domain
import oscar.cbls.core.computation.IntNotificationTarget
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.propagation.Checker

/**
 * Created by  Jannou Brohée on 3/10/16.
 */

object ForwardCulumativeIntegerIntegerDimensionOnVehicle {
  /**
   * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
   * @param content1AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param content2AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param default1ForUnroutedNodes is the content1 of a node that is not routed
   * @param default2ForUnroutedNodes is the content2 of a node that is not routed
   * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
   * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
   * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invairant
   * @return  (content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle) content at end is the content when back at the vehicle start
   */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Int,Int)=>(Int,Int),
            content1AtStart:Array[IntValue],
            content2AtStart:Array[IntValue],
            default1ForUnroutedNodes:Int,
            default2ForUnroutedNodes:Int,
            minContent:Int = 0,
            maxContent:Int = Int.MaxValue,
            contentName:String = "content"):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar]) ={

    val content1AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent).union(default1ForUnroutedNodes), contentName + "1 at node "+node))
    val content2AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent).union(default2ForUnroutedNodes), contentName + "2 at node "+node))

    val content1AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), contentName + "1 at end of route " + vehicle))
    val content2AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), contentName + "2 at end of route " + vehicle))

    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, n-1, "last point of vehicle" + vehicle))

    new ForwardCulumativeIntegerIntegerDimensionOnVehicle(routes,n,v,op,content1AtStart,content2AtStart,content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle,default1ForUnroutedNodes,default2ForUnroutedNodes)
    (content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle)
  }
}

class ForwardCulumativeIntegerIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                        n:Int,
                                                        v:Int,
                                                        op:(Int,Int,Int,Int)=>(Int,Int),
                                                        content1AtStart:Array[IntValue],
                                                        content2AtStart:Array[IntValue],
                                                        content1AtNode:Array[CBLSIntVar],
                                                        content2AtNode:Array[CBLSIntVar],
                                                        content1AtEnd:Array[CBLSIntVar],
                                                        content2AtEnd:Array[CBLSIntVar],
                                                        lastPointOfVehicle:Array[CBLSIntVar],
                                                        defaultVehicleContent1ForUnroutedNodes:Int,
                                                        defaultVehicleContent2ForUnroutedNodes:Int)

  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v) with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(content1AtStart)
  registerStaticAndDynamicDependencyArrayIndex(content2AtStart)
  finishInitialization()
  for(i <- content1AtNode) i.setDefiningInvariant(this)
  for(i <- content2AtNode) i.setDefiningInvariant(this)
  for(i <- content1AtEnd) i.setDefiningInvariant(this)
  for(i <- content2AtEnd) i.setDefiningInvariant(this)

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
    val newValue1 = content1AtStart(vehicle).value
    val newValue2 = content2AtStart(vehicle).value
    val oldValue1 = content1AtNode(vehicle).newValue
    val oldValue2 = content1AtNode(vehicle).newValue
    if(oldValue1 == newValue1 && oldValue2 == newValue2){
      false
    }else{
      content1AtNode(vehicle) := newValue1
      content2AtNode(vehicle) := newValue2
      true
    }
  }

  /**
   * @param prevNode
   * @param node
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtNode(prevNode : Int, node : Int) : Boolean = {
    val oldValue1 = content1AtNode(node).newValue
    val oldValue2 = content2AtNode(node).newValue
    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2) = op(prevNode,node,content1AtNode(prevNode).newValue,content2AtNode(prevNode).newValue)
    if(oldValue1 != newValue1 || oldValue2 != newValue2) {
      content1AtNode(node) := newValue1
      content2AtNode(node) := newValue2
      true
    }else{
      false
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int){
    lastPointOfVehicle(vehicle) := lastNode
    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2) = op(lastNode,vehicle,content1AtNode(lastNode).newValue,content2AtNode(lastNode).newValue)
    content1AtEnd(vehicle) := newValue1
    content2AtEnd(vehicle) := newValue2
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]){
    for(node <- unroutedNodes) {
      content1AtNode(node) := defaultVehicleContent1ForUnroutedNodes
      content2AtNode(node) := defaultVehicleContent2ForUnroutedNodes
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c,routes.value)
  }
  def check(c : Checker,s:IntSequence){

    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    def op2(fromNode:Int,toNode:Int,content:(Int,Int)) = op(fromNode,toNode,content._1,content._2)

    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) =
      AbstractVehicleCapacity.computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(
        n,
        v,
        op2,
        v => (content1AtStart(v).value,content2AtStart(v).value),
        s,
        (defaultVehicleContent1ForUnroutedNodes,defaultVehicleContent2ForUnroutedNodes))

    val currentVehicleLocation = this.toUpdateZonesAndVehicleStartAfter.get._2

    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == s.positionOfAnyOccurrence(vehicle).get,
        Some("Found start of vehicle(" + vehicle + "):=" + vehicleLocation.startPosOfVehicle(vehicle) +
          " should be :=" + s.positionOfAnyOccurrence(vehicle) +" seq :"+ s.mkString(",")))
      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }

    for(node <- 0 until n){
      c.check(nodeToContent(node) == (content1AtNode(node).newValue,content2AtNode(node).newValue),
        Some("Vehicle content at node(" + node + ") at pos : "+ s.positionsOfValue(node)+ " := " +
          (content1AtNode(node).newValue,content2AtNode(node).newValue) +
          " should be :=" + nodeToContent(node)+ " routes:" + s.mkString(",")))
    }

    for(vehicle <- 0 until v){
      c.check((content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue) == vehicleToContentAtEnd(vehicle),
        Some("Error on vehicle content at end vehicle:" + vehicle + " contentAtEnd(vehicle).newValue:" +
        (content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue) + " should be:" +  vehicleToContentAtEnd(vehicle)))
    }
  }
}

