package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
abstract class VRPExtension(vrp: VRP){

  vrp.addExtension(this)

  def preComputeRelevantNeighborsOfNode(node:Int, potentialRelevantNeighbors: List[Int]): List[Int]

  def postFilter(node: Int): (Int) => Boolean

}
