package oscar.cbls.routing.seq.neighborhood

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

import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.routing.model.HotSpottingInfo
import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.core.EasyNeighborhood
import oscar.cbls.search.move.Move

abstract class VRPSMove(override val objAfter: Int,
                       val neighborhood: EasyNeighborhood[_],
                       override val neighborhoodName:String = null, vrp:VRP)
  extends Move(objAfter, neighborhoodName) with HotSpottingInfo{

  override def touchedVariables: List[Variable] = List(vrp.seq)
}

abstract class EasyRoutingSNeighborhood extends EasyNeighborhood{

  def evalObjAndRollBack

}