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

import oscar.cbls._
import oscar.cbls.algo.hotSpot.HotSpotManager
import oscar.cbls.core.search.Move
import oscar.cbls.lib.search.neighborhoods.{AssignMove, SwapMove}

import scala.language.postfixOps

/**
  * this is a WarehouseLocation problem with a Tabu.
  * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
  * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
  * this set of variables is maintained through invariants
  */
object WarehouseLocationHotSpot extends App{

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
  val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse))

  val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

  m.close()


  val hotSpotManager = new HotSpotManager(W-1)
  hotSpotManager.enqueueAll()

  val neighborhood =(
    bestSlopeFirst(
      List(
        assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
        swapsNeighborhood(warehouseOpenArray, "SwapWarehouses"),

        assignNeighborhood(
          warehouseOpenArray,
          searchZone = {val hotSpotForSwitch = hotSpotManager.newExplorer; () => hotSpotForSwitch},
          hotRestart = false,
          name = "HotSwitch") //cannot replace the simple Switch

      ),refresh = W/10)
      onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/10), 2, obj)
      afterMoveOnMove {
        case a:AssignMove =>
          hotSpotManager.enqueue(a.id) //since we are using it for switch, we do not care.
          //Actually, we should enqueue the k-nearest warehouses of this one
        case s:SwapMove =>
          //Actually, we should enqueue the k-nearest warehouses of these to ones
          hotSpotManager.enqueue(s.idI)
          hotSpotManager.enqueue(s.idJ)
        case _ =>
          hotSpotManager.enqueueAll() //this is a bit overkil..
      })

  neighborhood.verbose = 1

  neighborhood.doAllMoves(obj=obj)

  println(openWarehouses)

}
