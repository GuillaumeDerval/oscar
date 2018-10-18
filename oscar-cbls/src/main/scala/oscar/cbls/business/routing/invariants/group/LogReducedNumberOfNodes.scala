package oscar.cbls.business.routing.invariants.group

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


import oscar.cbls.{CBLSIntVar, Variable}
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.ChangingSeqValue


case class NodesOnSubsequence(nbNodes:Int,
                              level:Int,
                              firstNode:Int,
                              lastNode:Int){
  require(nbNodes == (1 << level))
}

class LogReducedNumberOfNodes(routes:ChangingSeqValue, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends LogReducedGlobalConstraint[NodesOnSubsequence,Int](routes,v){
  /**
    * this method delivers the value of stepping from node "fromNode" to node "toNode.
    * you can consider that these two nodes are adjacent.
    *
    * @param fromNode
    * @param toNode
    * @return the type T associated with the step "fromNode -- toNode"
    */
  override def step(fromNode: Int, toNode: Int): NodesOnSubsequence = {
    NodesOnSubsequence(2, level = 1,fromNode,toNode)
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated with the first step followed by the second step
    */
  override def composeSteps(firstStep: NodesOnSubsequence, secondStep: NodesOnSubsequence): NodesOnSubsequence = {
    require(firstStep.level == secondStep.level)
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes, firstStep.level + 1,firstStep.firstNode,secondStep.lastNode)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int, segments: List[LogReducedSegment[NodesOnSubsequence]]): Int = {
    segments.map({
      case LogReducedPreComputedSubSequence(startNode, endNode, steps)=>
        require(steps.head.firstNode == startNode)
        require(steps.last.lastNode == endNode)
        steps.map(_.nbNodes).sum
      case LogReducedFlippedPreComputedSubSequence(startNode, endNode, steps) =>
        require(steps.head.firstNode == startNode)
        require(steps.last.lastNode == endNode)
        steps.map(_.nbNodes).sum
      case LogReducedNewNode(node) => 1
    }).sum
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Int): Unit = {
    nbNodesPerRoute(vehicle) := value
  }

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Int = ???

  override def outputVariables: Iterable[Variable] = nbNodesPerRoute
}