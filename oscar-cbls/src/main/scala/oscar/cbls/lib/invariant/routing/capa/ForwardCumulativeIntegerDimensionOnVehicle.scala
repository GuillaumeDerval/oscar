/*

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
import oscar.cbls.core.propagation.Checker
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
   * @param minContent Min content of a node
   * @param maxContent Max content of a node
   * @param maxStack Maximum depth of vehicleLocation history
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
            maxStack:Int = 4,
            contentName:String = "content"):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar]) ={
    val contentAtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent).union(defaultForUnroutedNodes), contentName + " at node "+node))
    val contentAtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), contentName + " at end of route " + vehicle))
    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, n-1, "last point of vehicle" + vehicle))

    new ForwardCumulativeIntegerDimensionOnVehicle(routes,n,v,op,contentAtStart,contentAtNode,contentAtEnd,lastPointOfVehicle,defaultForUnroutedNodes,maxStack)
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
                                                 defaultVehicleContentForUnroutedNodes:Int,
                                                 maxStack:Int = 4)
  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v,maxStack:Int){


  /**
   *
   * @param vehicle
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtStart(vehicle : Int) : Boolean = ???


  override def setVehicleContentAtNode(node : Int, newValueAtNode : Int) : Unit = super.setVehicleContentAtNode(node, newValueAtNode)

  override def checkInternals(c : Checker) : Unit = {
    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) = AbstractVehicleCapacity.computeNodeToIntContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(n,v,op,v => contentAtStart(v).value,routes.value,defaultVehicleContentForUnroutedNodes)
    for(node <- 0 until n){
      c.check(nodeToContent(node) == getVehicleContentAtNode(node),
        Some("Vehicle content at node(" + node + ") at pos : "+ routes.value.positionsOfValue(node)+ " := " + getVehicleContentAtNode(node) + " should be :=" + nodeToContent(node)+ " routes:" + routes.value.mkString(",")  + " contentAtStart:" + contentAtStart.mkString(",")))
    }
    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == routes.value.positionOfAnyOccurrence(vehicle).get,
        Some("Found start of vehicle(" + vehicle + "):=" + vehicleLocation.startPosOfVehicle(vehicle) + " should be :=" + routes.value.positionOfAnyOccurrence(vehicle) +" seq :"+routes.value.mkString(",")))
      c.check(contentAtEnd(vehicle).value == vehicleToContentAtEnd(vehicle))

      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }
  }
}



*/
