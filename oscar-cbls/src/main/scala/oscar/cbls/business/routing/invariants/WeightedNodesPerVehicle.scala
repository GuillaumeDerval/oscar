package oscar.cbls.business.routing.invariants

import oscar.cbls
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.{CBLSIntVar, Variable}


object WeightedNodesPerVehicle{
  /**
    * this constraints maintains the number of node per vehicle.
    *
    * @param routes the sequence representing the routes
    * @param v number of vehicle
    * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
    */
  def apply(routes:ChangingSeqValue, v : Int,  nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar]) =
    new WeightedNodesPerVehicle(routes, v , nodeWeight, weightPerVehicle)
}


/**
  * this constraints maintains the number of node per vehicle.
  *
  * @param routes the sequence representing the routes
  * @param v number of vehicle
  * @param weightPerVehicle an array telling how many nodes are reached per vehicle
  */
class WeightedNodesPerVehicle(routes:ChangingSeqValue, v : Int, nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar])
  extends GlobalConstraintDefinition[Long,Long](routes,v){

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