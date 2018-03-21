package oscar.cbls.test.routing.testcumul

import oscar.cbls.business.routing.invariants.{MovingVehicles, RouteSuccessorAndPredecessors}
import oscar.cbls.business.routing.invariants.capa.VehicleCapacityGlobalConstraint
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.lib.constraint.LE
import oscar.cbls.lib.search.combinators.{Mu, Profile, RoundRobin}

/**
  * Created by Quentin Meurisse on 23/10/17.
  */

class MySimpleRoutingWithVehicleCapacity(n: Int, v: Int, symmetricDistance:Array[Array[Int]], m:Store, maxPivot:Int, deltaAtNode:Array[Int], maxCapa:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted {

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val maxNodes = LE(Size(routes),n-3).violation

  val violation = Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, name = "violation of vehicle" + vehicle))
  val contentAtEndOfVehicleRoute = Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, value = deltaAtNode(vehicle), name = "content at end of vehicle" + vehicle))

  val contentConstraint = VehicleCapacityGlobalConstraint(routes, n, v, deltaAtNode, maxCapa, violation, contentAtEndOfVehicleRoute)
  val obj = new CascadingObjective(violation(0) + violation(1) + violation(2) + violation(3) + violation(4),
    new CascadingObjective(maxNodes,
      Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))))

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size


  val (next,prev) = RouteSuccessorAndPredecessors(routes,v,n)

  val movingVehicles = MovingVehicles(routes,v)

  override def toString : String = super.toString +
    "objective: " + obj.value + "\n" +
    "next: [" + next.map(_.value).mkString(",") + "]" + "\n" +
    "prev: [" + prev.map(_.value).mkString(",") + "]" + "\n" +
    "content: [" + contentAtEndOfVehicleRoute.mkString(",") + "]" + "\n" +
    "violation: " + violation.mkString("[", ",", "]") + "\n" +
    "routed:" + this.routed.value + "\n" +
    "unRouted:" + this.unrouted.value + "\n"
}

object BenchCapacityV2 {

  def run() = {
    val n = 30
    val v = 5
    val delta = Array(0,1,1,2,2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0)
    val maxPivotPerValuePercent = 4
    val maxcapa = 15

    val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)

    //val model = Store(checker = Some(ErrorChecker()))

    val model = Store()

    val myVRP = new MySimpleRoutingWithVehicleCapacity(n, v, symmetricDistanceMatrix, model, maxPivotPerValuePercent, delta, maxcapa)

    model.close()

    def routeUnroutedPoint(k:Int) =  InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,neighborhoodName = "InsertUF",best=true)

    //TODO: using post-filters on k-nearest is probably crap
    val routeUnroutedPoint2 =  Profile(InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

    def onePtMove(k:Int) = Profile(OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,best=true))

    val twoOpt = Profile(TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

    def threeOpt(k:Int, breakSym:Boolean) = Profile(ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

    val vlsn1pt = Profile(Mu[OnePointMoveMove](
      OnePointMove(myVRP.routed, () => myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP),
      l => Some(OnePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted),myVRP, hotRestart = false)),
      intermediaryStops = true,
      maxDepth = 3))


    val vlsnInsert = Mu[InsertPointMove](
      routeUnroutedPoint(3),
      l => if (myVRP.unroutedNodes.isEmpty) None else Some(routeUnroutedPoint(5)),
      intermediaryStops = false,
      maxDepth = 2)

    val remove = RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,best=true)
    def segExchange(k:Int) = SegmentExchange(myVRP,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted),() => myVRP.vehicles)

    val swapInOut = Profile((remove andThen routeUnroutedPoint(10)) name "SWAPInsert")
    val doubleInsert = Profile((routeUnroutedPoint(10) andThen routeUnroutedPoint(10)) name "doubleInsert")
    val doubleRemove = Profile( RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,best=true) andThen  RemovePoint(() => myVRP.routed.value.filter(_>=v), myVRP,best=true) name "doubleRemove")

    val search = new RoundRobin(List(onePtMove(100),doubleInsert,doubleRemove,swapInOut,vlsnInsert,threeOpt(5, breakSym = false),twoOpt,segExchange(10))) exhaust onePtMove(10) //(BestSlopeFirst(List(vlsnInsert, routeUnroutedPoint2, routeUnroutedPoint(10), swapInOut, onePtMove(10),twoOpt, threeOpt(10,true),vlsn1pt, routeUnroutedPoint)) exhaust threeOpt(20,true))// afterMove(/*myVRP.drawRoutes()*/)

    search.verbose = 0
    //search.verboseWithExtraInfo(3, ()=> "" + myVRP)

    search.doAllMoves(obj = myVRP.obj)
    model.propagate()
  }

  def main(args: Array[String]): Unit = {
    val n = 100
    for (i <- 0 to n){
      printf("run %3d: ", i)
      val startTime = System.currentTimeMillis()
      run()
      printf("%4d ms\n", System.currentTimeMillis() - startTime)
    }
  }

}
