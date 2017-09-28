package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.business.routing.model.VRP

/**
  * Created by fg on 12/09/17.
  */
class Capacity(vrp:VRP, contentFlow: Array[Int], forwardCumulativeConstraintOnVehicle: ForwardCumulativeConstraintOnVehicle){

  def enoughSpaceAfterNeighbor(node: Int) = (neighbor: Int) => {
    forwardCumulativeConstraintOnVehicle.freeSpaceAtNode(neighbor) <= contentFlow(node)
  }

}
