package oscar.examples.cbls.wlp

import oscar.cbls.invariants.core.computation.{CBLSIntConst, CBLSIntVar, Store}
import oscar.cbls.invariants.lib.logic.{Filter, SelectLESetQueue}
import oscar.cbls.invariants.lib.minmax.MinConstArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.AssignNeighborhood
import oscar.cbls.search.move.Move

import scala.language.postfixOps

/**
 * this is a WarehouseLocation problem with a Tabu.
 * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
 * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
 * this set of variables is maintained through invariants
 */
object WarehouseLocationTabu extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 15

  //the number of delivery points
  val D:Int = 150

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(w => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + w + "_open"))

  //We store in each warehouse variable its warehouse ID, using the
  // [[oscar.cbls.invariants.core.computation.DistributedStorageUtility]] mechanism
  //so we first ask a storageKey to the model
  val warehouseKey = m.newStorageKey()
  m.storeIndexesAt(warehouseOpenArray, warehouseKey)

  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArray(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  // we handle the tabu through invariants.
  // notice that they are completely dissociated from the rest of the model in this case.
  val TabuArray = Array.tabulate(W)(w => CBLSIntVar(m,0))
  val It = CBLSIntVar(m)
  val nonTabuWarehouses = SelectLESetQueue(TabuArray,It).setName("non tabu warehouses")

  m.close()

  //this composite neighborhood includes:
  // *the search part restricted to non tabu warehouses
  // *the update of the tabu and iteration count
  // *the stop criterion based on maxMoves since last improvement over obj
  // *the protection of the objectiveFunction
  val tabuTenure = 3
  val switchWithTabuNeighborhood = (AssignNeighborhood(warehouseOpenArray, "SwitchWarehouseTabu",
    searchZone = nonTabuWarehouses, best = true)
    beforeMove((mo:Move) => {
    for (v <- mo.touchedVariables) {
      TabuArray(v.getStorageAt[Int](warehouseKey)) := It.value + tabuTenure
    }
    It :+= 1 }) acceptAll() maxMoves W withoutImprovementOver obj saveBest obj restoreBestOnExhaust)

  switchWithTabuNeighborhood.verbose = 1

  //all moves are accepted because the neighborhood returns the best found move, and tabu might degrade obj.
  switchWithTabuNeighborhood.doAllMoves(obj=obj)

  println(openWarehouses)
}
