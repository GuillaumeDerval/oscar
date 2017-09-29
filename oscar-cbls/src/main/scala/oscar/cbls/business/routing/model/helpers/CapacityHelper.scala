package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle

/**
  * Created by fg on 12/09/17.
  */
object CapacityHelper{

  def enoughSpaceAfterNeighbor(node: Int,
                               contentFlow: Array[Int],
                               capacityInvariant: ForwardCumulativeConstraintOnVehicle): (Int) => Boolean =
    (neighbor: Int) => capacityInvariant.freeSpaceAtNode(neighbor) <= contentFlow(node)

}
