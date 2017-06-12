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

import java.awt.{Dimension, BorderLayout}
import javax.swing.JFrame

import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{CompositeMove, SupportForAndThenChaining, Neighborhood}
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.{ArgMin, MinConstArrayLazy}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{Profile, Mu, BestSlopeFirst}
import oscar.cbls.lib.search.neighborhoods.{AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.visual.wlp.{WareHouseLocationWindow, WareHouseLocationMap}

import scala.language.postfixOps

object WareHouseLocationVisu extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 200

  //the number of delivery points
  val D:Int = 1000

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost,warehousePositions,deliveryPositions,warehouseToWarehouseDistances) = WarehouseLocationGenerator.problemWithPositions(W,D,0,1000,3)

//  val costForOpeningWarehouse = Array.fill(W)(1000)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse))

  val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val visual = new WareHouseLocationWindow(deliveryPositions,warehousePositions,distanceCost,costForOpeningWarehouse)

  var bestObj = Int.MaxValue


  val k = 20
  //this is an array, that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
  val closestWarehouses = Array.tabulate(W)(warehouse =>
    KSmallest.lazySort(
      Array.tabulate(W)(warehouse => warehouse),
      otherwarehouse => warehouseToWarehouseDistances(warehouse)(otherwarehouse)
    ))

  //this procedure returns the k closest closed warehouses
  def kNearestClosedWarehouses(warehouse:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).value == 0)

  val neighborhood =(
    BestSlopeFirst(
      List(
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
          Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")),
          Profile(SwapsNeighborhood(warehouseOpenArray,
            searchZone1 = openWarehouses,
            searchZone2 = (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse),name = "Swap" + k + "Nearest", symmetryCanBeBrokenOnIndices = false))
      ),refresh = W/10)
      onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, W/10), 2, obj)
      onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, W/3), 1, obj))afterMove(
    if(obj.value < bestObj){
      bestObj = obj.value
      visual.redraw(openWarehouses.value)
    })

  neighborhood.verbose = 2

  neighborhood.doAllMoves(obj=obj)

  visual.redraw(openWarehouses.value)

  println(neighborhood.profilingStatistics)

  println(openWarehouses)
}

