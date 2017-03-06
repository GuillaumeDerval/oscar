package oscar.examples.cbls.wlp

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

import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.{MinConstArray, MinConstArrayLazy}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.Profile
import oscar.cbls.lib.search.neighborhoods.{AssignNeighborhood, SwapsNeighborhood}
import oscar.cbls.modeling.AlgebraTrait

import scala.language.postfixOps

object WarehouseLocationLazy extends App with AlgebraTrait{

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
  m.registerForPartialPropagation(openWarehouses)
  val openWarehouses2 = Filter(warehouseOpenArray).setName("openWarehouses")
  m.registerForPartialPropagation(openWarehouses2)

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArray(distanceCost(d), openWarehouses2, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses2))

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse))//.setName("distance_for_delivery_" + d))

  val objLazy = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val neighborhood = (Profile(
    Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"))
      step Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))))

  neighborhood.verbose = 0

  val startSolution = m.solution()
  m.propagate()

  neighborhood.doAllMoves(obj=obj)
  println(openWarehouses2)

  println(neighborhood.profilingStatistics)
  val nonLazyStats = neighborhood.profilingStatistics

  m.restoreSolution(startSolution)
  m.propagate()
  neighborhood.resetStatistics()
  neighborhood.reset()

  neighborhood.doAllMoves(obj=objLazy)
  println(openWarehouses)

  println("lazyStats:")
  println(neighborhood.profilingStatistics)

  println("non lazyStats:")
  println(nonLazyStats)

//  val nbAnihilation = distanceToNearestOpenWarehouseLazy.toList.map((a:MinConstArrayLazy) => a.nbAnihilation).sum
//  val nbDoIt = distanceToNearestOpenWarehouseLazy.toList.map((a:MinConstArrayLazy) => a.nbDoIt).sum
//  println("nbAnihilation : " + nbAnihilation)
//  println("nbDoIt: " + nbDoIt)
//  println("ratio:" + nbAnihilation.toDouble/(nbDoIt.toDouble + nbAnihilation.toDouble))
  //  println("model stats:")
  //  println(m.stats)

}
