package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.IntSequence

abstract class GlobalConstraintDefinition[T:Manifest,U:Manifest] {

  /**
   * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    * @param vehicle the vehicle where pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    */
  def performPreCompute(vehicle:Int,
                        routes:IntSequence,
                        setNodeValue:(Int,T) => Unit)

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route you are working on
    * @param routes the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  def computeVehicleValue(vehicle:Int,
                          segments:List[Segment],
                          routes:IntSequence,
                          nodeValue:Int=>Option[T]):U

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    * @param vehicle the vehicle number
    * @param value the value of the vehicle
    */
  def assignVehicleValue(vehicle:Int,value:U)

  //TODO: a non-incremental method that checks the output
}

sealed abstract class Segment()
class PreComputedSegment[T](startNode:Int,
                            startNodeValue:T,
                            endNode:Int,
                            endNodeValue:T,
                            isFLipped:Boolean) extends Segment
class NewNode(node:Int) extends Segment

