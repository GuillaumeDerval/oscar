package oscar.examples.visualfx

import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArrayValueWise
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.visualfx.wlp.WarehouseWindow

class WLP {

  val W:Int = 500

  val D:Int = 2000

  def optim(window: WarehouseWindow, distanceCost: Array[Array[Int]], costForOpeningWarehouse1: Array[Int], warehouseToWarehouseDistances:Array[Array[Int]]): Unit = {

    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    println("WarehouseLocation(W:" + W + ", D:" + D + ")")
    //the cost per delivery point if no location is open
    val defaultCostForNoOpenWarehouse = 10000

    val costForOpeningWarehouse =  Array.fill(W)(1000)

    val m = Store() //checker = Some(new ErrorChecker()))

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
    val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

    //val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    //  MinConstArrayLazy(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse))

    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
      new MinConstArrayValueWise(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse,maxDiameter = 2))




    val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))

    m.close()

    var bestObj = Int.MaxValue

    //this is an array, that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
    val closestWarehouses = Array.tabulate(W)(warehouse =>
      KSmallest.lazySort(
        Array.tabulate(W)(warehouse => warehouse),
        otherwarehouse => warehouseToWarehouseDistances(warehouse)(otherwarehouse)
      ))

    //this procedure returns the k closest closed warehouses
    def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)
    //this procedure returns the k closest open warehouses
    def kNearestOpenWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)
    def kNearestdWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse))

    def muLine(depth:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
      AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      (assignList:List[AssignMove]) =>
      {
        val lastChangedWarehouse = assignList.head.id
        val setTo = assignList.head.v
        val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
        Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
      },
      maxDepth = depth,
      intermediaryStops = true)


    def muStar(width:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
      AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      (assignList:List[AssignMove]) =>
      {
        val lastChangedWarehouse = assignList.last.id
        val setTo = assignList.head.v
        val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
        Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
      },
      maxDepth = width,
      intermediaryStops = true)

    def swapsK(k:Int,openWarehoueseTocConsider:()=>Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
      searchZone1 = openWarehoueseTocConsider,
      searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
      name = "Swap" + k + "Nearest",
      symmetryCanBeBrokenOnIndices = false)

    def doubleSwap(k:Int) = (swapsK(k) dynAndThen((firstSwap:SwapMove) => swapsK(k,() => kNearestOpenWarehouses(firstSwap.idI,k)))) name "DoubleSwap"

    val neighborhood =(
      (BestSlopeFirst(
        List(
          Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
          Profile(swapsK(20) guard(() => openWarehouses.value.size >= 5)), //we set a minimal size because the KNearest is very expensive if the size is small
          Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") guard(() => openWarehouses.value.size >= 5))
        ),refresh = W/10)
        onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => openWarehouses.value.size/5), 2, obj)
      ) exhaust (Profile(muLine(3,3,15)) exhaustAndContinueIfMovesFound Profile(muLine(4,3,15)))) afterMove {
      window.update(openWarehouses.value)
    }

    neighborhood.verbose = 2

    neighborhood.doAllMoves(obj=obj)

    window.update(openWarehouses.value,true)
    window.TPE.shutdown()
    //println(neighborhood.profilingStatistics)

    println(openWarehouses.value)
  }

}
