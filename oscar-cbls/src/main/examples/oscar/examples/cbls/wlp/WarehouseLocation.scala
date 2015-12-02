/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cbls.wlp

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.minmax.{MinConstArrayLazy, MinConstArray}
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.combinators.{LearningRandom, BiasedRandom, Profile}
import oscar.cbls.search._

import scala.language.postfixOps

object WarehouseLocation extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 300

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse))

  val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val neighborhood = (AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, W/10) maxMoves 2)
    saveBestAndRestoreOnExhaust obj)

  neighborhood.verbose = 1
  
  neighborhood.doAllMoves(obj=obj)

  println(openWarehouses)

  //  println("model stats:")
//  println(m.stats)

  //TODO: how about a k-nearest for the swap, in a first fast version of the neighborhood?
}
