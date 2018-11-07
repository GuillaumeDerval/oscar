package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.TimeWindowConstraint
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.Best

object VRPWithOnlyTimeWindow extends App {

  new VRPWithOnlyTimeWindow(true)

}

class VRPWithOnlyTimeWindow(oldVersion: Boolean){
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  var (earlylines, deadlines, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix)
  deadlines = deadlines.take(v).map(x => Math.min(deadlines.drop(v).max*2, Int.MaxValue/10*9)) ++ deadlines.drop(v)

  val myVRP =  new VRP(m,n,v)

  // Distance
  val totalRouteLength = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)

  //Time window
  val timeWindowExtension = timeWindow(earlylines,deadlines,taskDurations,maxWaitingDurations)

  val obj: CascadingObjective =
    if(oldVersion){
      val oldTimeWindowInvariant = forwardCumulativeIntegerIntegerDimensionOnVehicle(
        myVRP.routes,n,v,
        (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
          val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
          val leaveTimeAtToNode =
            if(toNode < v) 0
            else Math.max(arrivalTimeAtToNode,earlylines(toNode)) + taskDurations(toNode)
          (arrivalTimeAtToNode,leaveTimeAtToNode)
        },
        Array.tabulate(v)(x => new CBLSIntConst(0)),
        Array.tabulate(v)(x => new CBLSIntConst(earlylines(x)+taskDurations(x))),
        0,
        0,
        contentName = "Time at node"
      )
      val constraints = new ConstraintSystem(myVRP.routes.model)
      val arrivalTimes = oldTimeWindowInvariant.content1AtNode
      val leaveTimes = oldTimeWindowInvariant.content2AtNode
      val arrivalTimesAtEnd = oldTimeWindowInvariant.content1AtEnd
      for(i <- 0 until n){
        if(i < v && deadlines(i) != Int.MaxValue) {
          constraints.post(arrivalTimesAtEnd(i).le(deadlines(i)).nameConstraint("end of time for vehicle " + i))
        } else {
          if(deadlines(i) != Int.MaxValue)
            constraints.post(leaveTimes(i).le(deadlines(i)).nameConstraint("end of time window on node " + i))
          if(maxWaitingDurations(i) != Int.MaxValue)
            constraints.post(arrivalTimes(i).ge(earlylines(i)).nameConstraint("start of time window on node (with duration)" + i))
        }
      }

      new CascadingObjective(constraints,
        totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes))))
    }
    else{
      val violations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
      val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix.getTravelDuration(from, 0, to)))
      val smartTimeWindowInvariant =
        TimeWindowConstraint(myVRP.routes, n, v,
          timeWindowExtension.earlylines,
          (timeWindowExtension.deadlines, timeWindowExtension.taskDurations).zipped.map(_ - _),
          timeWindowExtension.taskDurations,
          timeMatrix, violations)

      new CascadingObjective(sum(violations),
        totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes))))
    }

  //Next & prev
  val routeForNextPrev = myVRP.routes.createClone()
  val (next,prev) = (Array.tabulate(routeForNextPrev.maxValue + 1)(node =>
    CBLSIntVar(m,n,name="successor of node" + node)),
    Array.tabulate(routeForNextPrev.maxValue + 1)(node =>
      CBLSIntVar(m,n,name="predecessor of node" + node)))
  routeSuccessorAndPredecessors(routeForNextPrev,v,n)(next,prev)

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val isARelevantSuccessorsOfNodes = Array.tabulate(n)(node => Array.tabulate(n)(x => relevantSuccessorsOfNodes(node).contains(x)))

  def postFilter(node:Int): (Int) => Boolean = {
    (neighbor: Int) => {
      val successor = next(neighbor).value
      myVRP.isRouted(neighbor) &&
        isARelevantSuccessorsOfNodes(node)(successor)
    }
  }

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes))

  val insertPoint = insertPointUnroutedFirst(
    () => myVRP.unrouted.value,
    ()=> myVRP.kFirst(v*4,closestRelevantPredecessorsByDistance, postFilter),
    myVRP,
    //selectInsertionPointBehavior = Best(),
    neighborhoodName = "InsertUF")

  val onePtMove = profile(onePointMove(
    () => myVRP.routed.value,
    ()=> myVRP.kFirst(n,closestRelevantPredecessorsByDistance,postFilter),
    myVRP,
    //selectDestinationBehavior = Best(),
    neighborhoodName = "OnePtMove"))

  def threeOptMove(k: Int) = profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantPredecessorsByDistance,postFilter), myVRP,neighborhoodName = "ThreeOpt(k=" + k + ")"))


  //val search = andThen(insertPoint,onePtMove)
  val search = profile(insertPoint)
  //val search = profile(insertPoint) exhaust onePtMove
  //val search = profile(insertPoint) exhaust threeOptMove(v*2) exhaust onePtMove

  search.verbose = 1

  search.doAllMoves(obj=obj)

  println(myVRP.routes)

  println(obj)

  println(myVRP)

  println(search.profilingStatistics)

}
