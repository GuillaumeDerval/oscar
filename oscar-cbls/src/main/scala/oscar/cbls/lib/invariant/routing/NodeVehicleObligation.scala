package oscar.cbls.lib.invariant.routing

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls._
import oscar.cbls.core._

object NodeVehicleObligation{
  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of obligation node->set of vehicles (it must reach one vehicle among any of the given set, or be non-routed)
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * we consider that a node that is not routed does not violate the obligation constraint
   * @param routes
   * @param v the number of vehicles
   * @param n the number of nodes
   * @param nodeVehicleObligation the obligation that we are monitoring
   * @return an array telling the violation per vehicle
   * @note this is a preliminary naive version of the constraint. a faster one is to be developed!
   */
  def apply(routes:ChangingSeqValue,v:Int, n:Int, nodeVehicleObligation:Map[Int,Set[Int]]):Array[CBLSIntVar] = {
    val violationPerVehicle =  Array.tabulate(v)(vehicle => CBLSIntVar(routes.model,name="violation of NodeVehicleObligation for vehicle" + vehicle))

    val vehicles = 0 until v

    var nodeVehicleRestrictions:List[(Int,Int)] = List.empty

    for((node,vehicleObligations) <- nodeVehicleObligation){
      if(vehicleObligations.size < v){
        val forbiddenVehicles = vehicles.filterNot(vehicleObligations)
        for(forbiddenVehicle <- forbiddenVehicles){
          nodeVehicleRestrictions = (node,forbiddenVehicle) :: nodeVehicleRestrictions
        }
      }
    }

    new NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions, violationPerVehicle)

    violationPerVehicle
  }
}
