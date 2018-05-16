package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
object CapacityHelper{

  /**
    * This method generate a method that can be used to determine whether or not their is enough space to insert a node after a neighor.
    * It first generate an array of Int representing the freeSpace at each node of the problem.
    * If the node isn't routed there is no space.
    *
    * NB: You should call this method after each movement to update the freeSpace value.
    *     And avoid calling it each time you want to filter a move.
    *
    * @param n The amount of node in the problem
    * @param capacityInvariant A ForwardCumulativeConstraintOnVehicle representing the content invariant
    * @return a method (Int,Int,Array[Int]) => Boolean
    */
  def enoughSpaceAfterNeighbor(n: Int, capacityInvariant: ForwardCumulativeConstraintOnVehicle): (Int,Int,Array[Int]) => Boolean ={
    val freeSpaceAtNodeNow = capacityInvariant.freeSpaceAtNodes
    (node: Int, neighbor: Int, contentsFlow: Array[Int]) => freeSpaceAtNodeNow(neighbor) >= contentsFlow(node)
  }

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
