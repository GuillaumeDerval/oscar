package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
object CapacityHelper{

  /**
    *
    * @param node
    * @param contentsFlow
    * @param capacityInvariant
    * @return
    */
  def enoughSpaceAfterNeighbor(node: Int,
                               contentsFlow: Array[Int],
                               capacityInvariant: ForwardCumulativeConstraintOnVehicle): (Int) => Boolean =
    (neighbor: Int) => capacityInvariant.freeSpaceAtNode(neighbor) >= contentsFlow(node)

  /**
    *
    * @param vrp
    * @param maxCapacity
    * @param contentsFlow
    * @return
    */
  def relevantPredecessorsOfNodes(vrp: VRP, maxCapacity : Int, vehiclesSize: Array[Int], contentsFlow: Array[Int]): Map[Int,HashSet[Int]] ={
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect {
      case predecessor if
      ((if(predecessor < vrp.v) maxCapacity - vehiclesSize(predecessor) else contentsFlow(predecessor)) + contentsFlow(node)) <= maxCapacity &&
        predecessor != node => predecessor
    }: _*)).toMap
  }

}
