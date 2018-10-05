package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.ChangingSeqValue

abstract class GlobalConstraintDefinition[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int) extends PreComputeInvariant[T,U](routes,v) {

  /**
   * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    * @param vehicle the vehicle where pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    * @param getNodeValue a method that you can use to get the value associated wit ha node
    *                     BEWARE: you have zero info on when it can generated, so only query the value
    *                     that you have just set through the method setNodeValue.
    *                     also, you should only query the value of node in the route of vehicle "vehicle"
    */
  def performPreCompute(vehicle:Int,
                        routes:IntSequence,
                        preComputedVals:Array[T])

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  def computeVehicleValue(vehicle:Int,
                          segments:List[Segment[T]],
                          routes:IntSequence,
                          PreComputedVals:Array[T]):U

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    * @param vehicle the vehicle number
    * @param value the value of the vehicle
    */
  def assignVehicleValue(vehicle:Int,value:U): Unit

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  def computeVehicleValueFromScratch(vehicle : Int, routes : IntSequence):U
}

sealed abstract class Segment[T]()

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was present in the global sequence when the pre-computation was performed
  * @param startNode the first node of the subsequence
  * @param startNodeValue the T value that the pre-computation associated with the node "startNode"
  * @param endNode the last node of the subsequence
  * @param endNodeValue the T value that the pre-computation associated with the node "endNode"
  * @tparam T the type of precomputation
  */
case class PreComputedSubSequence[T](startNode:Int,
                            startNodeValue:T,
                            endNode:Int,
                            endNodeValue:T) extends Segment[T]{
  override def toString: String = {
    "PreComputedSubSequece (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + ")"
  }
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was not present in the global sequence when the pre-computation was performed, but
  * the flippedd subsequence obtained by flippig it was present in the global sequence when the pre-computation was performed, but
  * @param startNode the first node of the subsequence (it was after the endNode when pre-computation ws performed)
  * @param startNodeValue the T value that the pre-computation associated with the node "startNode"
  * @param endNode the last node of the subsequence (it was before the endNode when pre-computation ws performed)
  * @param endNodeValue the T value that the pre-computation associated with the node "endNode"
  * @tparam T the type of precomputation
  */
case class FlippedPreComputedSubSequence[T](startNode:Int,
                            startNodeValue:T,
                            endNode:Int,
                            endNodeValue:T) extends Segment[T]{
  override def toString: String = {
    "FlippedPreComputedSubSequece (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + ")"
  }
}

/**
  * This represent that a node that was not present in the initial sequence when pre-computation was performed.
  * @param node
  */
case class NewNode[T](node:Int) extends Segment[T]{
  override def toString: String = {
    "NewNode - Node : " + node
  }
}

