package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.lib.invariant.routing.capa.ForwardCumulativeConstraintOnVehicle

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
class Capacity(vrp:VRP, contentFlow: Array[Int], forwardCumulativeConstraintOnVehicle: ForwardCumulativeConstraintOnVehicle) extends VRPExtension(vrp){

  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: HashSet[Int]): HashSet[Int] = {
    potentialRelevantNeighbors
  }

  override def postFilter(node: Int) = (neighbor: Int) => {
    forwardCumulativeConstraintOnVehicle.freeSpaceAtNode(neighbor) <= contentFlow(node)
  }
}

class CapacityExtensionBuilder(vrp: VRP, capacityInvariant: ForwardCumulativeConstraintOnVehicle){

  private val contentFlow: Array[Int] = Array.fill(vrp.n)(0)

  def setContentFlow(contentFlow: Array[Int]): Unit ={
    for(i <- contentFlow.indices)
      this.contentFlow(i) = contentFlow(i)
  }

  def build(): Capacity ={
    new Capacity(vrp, contentFlow, capacityInvariant)
  }
}
