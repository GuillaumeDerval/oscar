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
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls._


object NbNodes{
  /**
    * this constraints maintains the number of node per vehicle.
    *
    * @param routes the sequence representing the routes
    * @param v number of vehicle
    * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
    */
  def apply(routes:ChangingSeqValue, v : Int, nbNodesPerVehicle : Array[CBLSIntVar]) =
    new NbNodes(routes, v , nbNodesPerVehicle)
}


/**
  * this constraints maintains the number of node per vehicle.
  *
  * @param routes the sequence representing the routes
  * @param v number of vehicle
  * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
  */
class NbNodes(routes:ChangingSeqValue, v : Int, nbNodesPerVehicle : Array[CBLSIntVar])
  extends GlobalConstraintDefinition[Option[Long],Long](routes,v){

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle      the vehicle where pre-computation must be performed
    * @param routes       the sequence representing the route of all vehicle
    *                     BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    * @param getNodeValue a method that you can use to get the value associated wit ha node
    *                     BEWARE: you have zero info on when it can generated, so only query the value
    *                     that you have just set through the method setNodeValue.
    *                     also, you should only query the value of node in the route of vehicle "vehicle"
    */
  override def performPreCompute(vehicle: Long, routes: IntSequence, preComputedVals: Array[Option[Long]]): Unit = {
    var nbNode = 0
    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            nbNode = nbNode + 1
            preComputedVals(elem.value) = Some(nbNode)
          }
          vExplorer = elem.next
      }
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Long, segments: List[Segment[Option[Long]]], routes: IntSequence, PreComputedVals: Array[Option[Long]]): Long = {
    val tmp = segments.map(
      _ match {
        case PreComputedSubSequence (fstNode, fstValue, lstNode, lstValue, length) =>
          lstValue.get - fstValue.get + 1
        case FlippedPreComputedSubSequence(lstNode,lstValue,fstNode,fstValue, length) =>
          lstValue.get - fstValue.get + 1
        case NewNode(_) =>
          1
      }).sum
    //println("Vehicle : " + vehicle + "--" + segments.mkString(","))
    tmp
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
    nbNodesPerVehicle(vehicle) := value
  }


  def countVehicleNode(vehicle : Long,vExplorer : Option[IntSequenceExplorer]) : Long = {
    vExplorer match {
      case None => 0
      case Some(elem) =>
        if (elem.value < v) 0 else (1 + countVehicleNode(vehicle,elem.next))
    }
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence): Long = {
    1 + countVehicleNode(vehicle,routes.explorerAtAnyOccurrence(vehicle).get.next)
  }

  override def outputVariables: Iterable[Variable] = {
    nbNodesPerVehicle
  }
}

case class NodesOnSubsequence(nbNodes:Long,
                              level:Long,
                              firstNode:Long,
                              lastNode:Long){
  //  require(nbNodes == ((1L << (level-1L))  -1L))

  override def toString: String = {
    "NodesOnSubsequence(nbNodes:" + nbNodes + ",level:" + level + ",firstNode:" + firstNode + ",lastNode:" + lastNode + ")"
  }
}


@deprecated("This is for example only, do not use this version","")
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

@deprecated("This is for example only, do not use this version","")
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

