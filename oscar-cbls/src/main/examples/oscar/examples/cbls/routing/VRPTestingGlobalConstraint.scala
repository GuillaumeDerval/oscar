package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.IntVarOps
import oscar.cbls.algo.seq._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.business.routing.neighborhood.TwoOpt
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.core.search.Best
import oscar.cbls.lib.constraint.LE

class NbNodeGlobalConstraint(routes:ChangingSeqValue,v : Int,nbNodesPerVehicle : Array[CBLSIntVar]) extends GlobalConstraintDefinition[Int,Int](routes,v){



  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle      the vehicle where pre-computation must be performed
    * @param routes       the sequence representing the route of all vehicle
    *                     BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    * @param getNodeValue a method that you can use to get the value associated wit ha node
    *                     BEWARE: you have zero info on when it can generated, so only query the value
    *                     that you have just set through the method setNodeValue.
    *                     also, you should only query the value of node in the route of vehicle "vehicle"
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[Int]): Unit = {
    println("PreCompute For Vehicle : " + vehicle + " : " + routes.toString)
    var nbNode = 0
    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value == vehicle + 1){
            continue = false
          } else {
            nbNode = nbNode + 1
            preComputedVals(elem.value) = nbNode
          }
          vExplorer = elem.next
      }
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[Int]], routes: IntSequence, PreComputedVals: Array[Int]): Int = {
    println("ComputeVehicleValues : " + segments.mkString(","))
    val tmp = segments.map(
      _ match {
      case PreComputedSubSequence (fstNode, fstValue, lstNode, lstValue) =>
        println(lstValue - fstValue + 1)
        lstValue - fstValue + 1
      case FlippedPreComputedSubSequence(lstNode,lstValue,fstNode,fstValue) =>
        println(lstValue - fstValue + 1)
        lstValue - fstValue + 1
      case NewNode(_) =>
        1
    }).sum

    println("Out = " + tmp)
    tmp
  }



  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Int): Unit = {
    nbNodesPerVehicle(vehicle) := value
  }


  def countVehicleNode(vehicle : Int,vExplorer : Option[IntSequenceExplorer]) : Int = {
    println(vehicle + ",")
    vExplorer match {
      case None => 0
      case Some(elem) =>
        if (elem.value < v) 0 else (1 + countVehicleNode(vehicle,elem.next))
    }
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Int = {
    1 + countVehicleNode(vehicle,routes.explorerAtAnyOccurrence(vehicle).get.next)
  }

  override def outputVariables: Iterable[Variable] = {
    nbNodesPerVehicle
  }
}

object VRPTestingGlobalConstraint extends App {


  val nbNode = 100;
  val nbVehicle = 10;
  val model = new Store(checker = Some(new ErrorChecker))
  //val model = new Store()

  val problem = new VRP(model,nbNode,nbVehicle)

  val (symetricDistanceMatrix,_) = RoutingMatrixGenerator(nbNode)

  val nbNodesPerVehicle : Array[CBLSIntVar] = Array.tabulate(nbVehicle)({_ => CBLSIntVar(model,0)})

  val routeLengthPerVehicle = constantRoutingDistance(problem.routes,nbNode,nbVehicle,perVehicle = true,symetricDistanceMatrix,true,true,false)

  val totalRouteLength = sum(routeLengthPerVehicle)

  val nbNodeConstraint = new NbNodeGlobalConstraint(problem.routes,nbVehicle,nbNodesPerVehicle)


  val c = new ConstraintSystem(model)

  for(vehicle <- 0 until nbVehicle){
    c.add(nbNodesPerVehicle(vehicle) le 10)
  }

  c.close()

   model.close()

  val closestRelevantNeighbors = Array.tabulate(nbNode)(DistanceHelper.lazyClosestPredecessorsOfNode(symetricDistanceMatrix,_ => problem.nodes))


  val routeUnroutedPoint =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => problem.kFirst(10,closestRelevantNeighbors,_ => node => problem.isRouted(node)),
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR 1"))


  /*val routeUnroutedPoint =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => _ => problem.routes.value,
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR"))*/

  val onePtMove =
    profile(onePointMove(problem.routed,
      () => problem.kFirst(10,closestRelevantNeighbors,_ => node => problem.isRouted(node)),
      problem))

  val twoOpt =
    profile(TwoOpt(problem.routed,
      () => problem.kFirst(10,closestRelevantNeighbors,_ => node => problem.isRouted(node)),
      problem))

  val search =
    bestSlopeFirst(List(routeUnroutedPoint,
      onePtMove,
      twoOpt))

  search.verbose = 3

  search.doAllMoves(obj = obj)

  println(problem)
  println(totalRouteLength)
  println(search.profilingStatistics)

}
