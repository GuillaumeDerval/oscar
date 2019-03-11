package oscar.cbls.business.routing.invariants

import oscar.cbls
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.{CBLSIntVar, Variable}


object WeigthedNodesPerVehicle{
  /**
    * this constraints maintains the number of node per vehicle.
    *
    * @param routes the sequence representing the routes
    * @param v number of vehicle
    * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
    */
  def apply(routes:ChangingSeqValue, v : Int,  nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar]) =
    new WeigthedNodesPerVehicle(routes, v , nodeWeight, weightPerVehicle)
}


/**
  * this constraints maintains the number of node per vehicle.
  *
  * @param routes the sequence representing the routes
  * @param v number of vehicle
  * @param weightPerVehicle an array telling how many nodes are reached per vehicle
  */
class WeigthedNodesPerVehicle(routes:ChangingSeqValue, v : Int, nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar])
  extends GlobalConstraintDefinition[Long,Long](routes,v){

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
  override def performPreCompute(vehicle: Long, routes: IntSequence, preComputedVals: Array[Long]): Unit = {
    var cumulatedWeight = 0L
    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            cumulatedWeight = cumulatedWeight + nodeWeight(cbls.longToInt(elem.value))
            preComputedVals(cbls.longToInt(elem.value)) = cumulatedWeight
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
  override def computeVehicleValue(vehicle: Long, segments: List[Segment[Long]], routes: IntSequence, PreComputedVals: Array[Long]): Long = {
    val tmp = segments.map(
      _ match {
        case PreComputedSubSequence (fstNode, fstValue, lstNode, lstValue) =>
          lstValue - fstValue + nodeWeight(cbls.longToInt(fstNode))
        case FlippedPreComputedSubSequence(lstNode,lstValue,fstNode,fstValue) =>
          lstValue - fstValue + nodeWeight(cbls.longToInt(fstNode))
        case NewNode(node) =>
          nodeWeight(cbls.longToInt(node))
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
    weightPerVehicle(cbls.longToInt(vehicle)) := value
  }

  def computeRouteWeight(vehicle : Long,vExplorer : Option[IntSequenceExplorer]) : Long = {
    vExplorer match {
      case None => 0
      case Some(elem) =>
        if (elem.value < v) 0
        else nodeWeight(cbls.longToInt(elem.value)) + computeRouteWeight(vehicle,elem.next)
    }
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence): Long = {
    nodeWeight(cbls.longToInt(vehicle)) + computeRouteWeight(vehicle,routes.explorerAtAnyOccurrence(vehicle).get.next)
  }

  override def outputVariables: Iterable[Variable] = {
    weightPerVehicle
  }
}