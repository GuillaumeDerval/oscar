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


import oscar.cbls.algo.quick.QList
import oscar.cbls.{CBLSIntVar, Variable}
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue
import oscar.cbls._

case class NodesOnSubsequence(nbNodes:Long,
                              level:Long,
                              firstNode:Long,
                              lastNode:Long){
//  require(nbNodes == ((1L << (level-1L))  -1L))

  override def toString: String = {
    "NodesOnSubsequence(nbNodes:" + nbNodes + ",level:" + level + ",firstNode:" + firstNode + ",lastNode:" + lastNode + ")"
  }
}

class LogReducedNumberOfNodes(routes:ChangingSeqValue, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends LogReducedGlobalConstraint[NodesOnSubsequence,Long](routes,v){

  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Long): NodesOnSubsequence = {
    NodesOnSubsequence(1L, level = 0L,node,node)
  }


  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Long): NodesOnSubsequence = {
    NodesOnSubsequence(0L, level = 0L, vehicle, vehicle) //since it is the number of nodes, return is not counted as a node
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
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes, firstStep.level + 1L,firstStep.firstNode,secondStep.lastNode)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Long, segments: QList[LogReducedSegment[NodesOnSubsequence]]): Long = {
    segments.qMap({
      case s@LogReducedPreComputedSubSequence(startNode, endNode, steps)=>
        //require(steps.isEmpty || steps.head.firstNode == startNode)
        //require(steps.isEmpty || steps.last.lastNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedFlippedPreComputedSubSequence(startNode, endNode, steps) =>
        //require(steps.isEmpty || steps.last.lastNode == startNode, steps)
        //require(steps.isEmpty || steps.head.firstNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedNewNode(_, _) =>
        1L
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
  override def assignVehicleValue(vehicle: Long, value: Long): Unit = {
    nbNodesPerRoute(vehicle) := value
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence): Long = {
    if(vehicle == v-1L){
      routes.size - routes.positionOfAnyOccurrence(vehicle).get
    }else{
      routes.positionOfAnyOccurrence(vehicle+1L).get - routes.positionOfAnyOccurrence(vehicle).get
    }
  }

  override def outputVariables: Iterable[Variable] = nbNodesPerRoute
}


class LogReducedNumberOfNodesWithExtremes(routes:ChangingSeqValue, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends LogReducedGlobalConstraintWithExtremes[NodesOnSubsequence,Long](routes,v){
  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Long): NodesOnSubsequence = {
    NodesOnSubsequence(1L, level = 0L,node,node)
  }


  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Long): NodesOnSubsequence = {
    NodesOnSubsequence(0L, level = 0L, vehicle, vehicle) //since it is the number of nodes, return is not counted as a node
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated with the first step followed by the second step
    */
  override def composeSteps(firstStep: NodesOnSubsequence, secondStep: NodesOnSubsequence): NodesOnSubsequence = {
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes, firstStep.level + 1L,firstStep.firstNode,secondStep.lastNode)
  }


  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Long, segments: QList[LogReducedSegment[NodesOnSubsequence]]): Long = {
    segments.qMap({
      case s@LogReducedPreComputedSubSequence(startNode, endNode, steps)=>
        //require(steps.isEmpty || steps.head.firstNode == startNode)
        //require(steps.isEmpty || steps.last.lastNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedFlippedPreComputedSubSequence(startNode, endNode, steps) =>
        //require(steps.isEmpty || steps.last.lastNode == startNode, steps)
        //require(steps.isEmpty || steps.head.firstNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedNewNode(_, _) =>
        1L
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
  override def assignVehicleValue(vehicle: Long, value: Long): Unit = {
    nbNodesPerRoute(vehicle) := value
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence): Long = {
    if(vehicle == v-1L){
      routes.size - routes.positionOfAnyOccurrence(vehicle).get
    }else{
      routes.positionOfAnyOccurrence(vehicle+1L).get - routes.positionOfAnyOccurrence(vehicle).get
    }
  }

  override def outputVariables: Iterable[Variable] = nbNodesPerRoute
}

