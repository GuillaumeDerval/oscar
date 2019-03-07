package oscar.examples.cbls.routing.drone

import oscar.cbls
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

/**
  *
  * @param id the node associated with the type T
  * @param deltaE drone consumption to this node
  * @param m drone weight at this node
  * @param sumDm the sum of the distances and masses to this node
  * @param sumD the sum of the distances to this node
  * @param revDeltaE the consumption of the drone at this node while traveling the road in the opposite direction
  * @param revM drone weight at this node while traveling the road in the opposite direction
  * @param revSumDm the sum of the distances and masses while traveling the road in the opposite direction
  * @param revSumD the sum of the distances to this node while traveling the road in the opposite direction
  */
case class QuadrativeDroneAutonomy(id: Int,
                                   deltaE: Long,
                                   m: Long,
                                   sumDm: Long,
                                   sumD: Long,
                                   revDeltaE: Long,
                                   revM: Long,
                                   revSumDm: Long,
                                   revSumD: Long) {

  def printPCV() : Unit = {
    printf("Node : "+ id +
      "\n dE : "+ deltaE +
      "\n m : "+ m +
      "\n sum(dm) : "+ sumDm +
      "\n sum(d) : "+ sumD +
      "\n revDE : "+ revDeltaE +
      "\n revM : "+ revM  +
      "\n revSum(dm) : " + revSumDm +
      "\n revSum(d) : " + revSumD)
  }
}

case class EnergyAndWeight(vehicle: Int,
                           weight: Long,
                           energyNeeded: Long)

/**
  *This global constraint computes for each vehicle the value
  * sum_(hops) (d(i,j)*(amj² + bmj + c))
  *   where d is the distance of each hop,
  *         mj is the weight of the drone after each hop, knowing that this mass varies at each point
  *
  * @param a the coefficient a of the energy consumption formula the drone: E(m) = am² + bm + c
  * @param b the coefficient b of the energy consumption formula the drone: E(m) = am² + bm + c
  * @param c the coefficient c of the energy consumption formula the drone: E(m) = am² + bm + c
  * @param v the number of vehicle in the model
  * @param DistanceMatrix the distance matrix for computing the d(i,j)
  * @param droppedWeights the list of masses dropped for each node
  * @param droneWeight weight of the drone when empty, shared by all vehicles
  * @param routeEnergy the list of energy required for each vehicle
  * @param routeWeight: Array[CBLSIntVar],
  */

class AutonomyGlobalConstraint(a: Long, b: Long, c: Long,
                               v: Int,
                               DistanceMatrix: Array[Array[Long]],
                               droppedWeights: Array[Long],
                               droneWeight: Long,
                               routeEnergy: Array[CBLSIntVar],
                               routeWeight: Array[CBLSIntVar],
                               routes: ChangingSeqValue)
  extends GlobalConstraintDefinition[QuadrativeDroneAutonomy, EnergyAndWeight](routes,v) {

  // ///////////////////////////////////////////////////////////
  // fundamental algebraic operation on droneState and segments

  /**
    *
    * @param weight weight of the drone when leaving this currentPoint
    * @param currentPoint the node associated with the DroneState
    * @param energyForComingRoute energy needed for finish the road at the currentPoint
    */
  case class DroneState(weight:Long,
                        currentPoint:Int,
                        energyForComingRoute:Long)

  def outputVariables : Iterable[CBLSIntVar]  = routeEnergy

  /**
    *
    * @param state the current state of the drone
    * @param prevNode the node we insert that precede the state
    * @return the new state of the drone at prevNode
    */
  private def stepBackward(state:DroneState,prevNode:Int):DroneState = {
    val prevWeight = state.weight+droppedWeights(cbls.longToInt(state.currentPoint))
    DroneState(prevWeight,
      prevNode,
      state.energyForComingRoute + DistanceMatrix(prevNode)(cbls.longToInt(state.currentPoint))*(a*prevWeight*prevWeight + b*prevWeight + c))
  }

  /**
    *
    * @param state the current state of the drone
    * @param prevSeg the segment of nodes that we insert before the state
    * @return the new state of the drone at the beginning of prevSeg
    */
  private def bigStepBackward(state:DroneState,prevSeg:PreComputedSubSequence[QuadrativeDroneAutonomy]):DroneState = {

    val deltaDropped = state.weight + droppedWeights(state.currentPoint) - prevSeg.endNodeValue.m
    val prevWeight = prevSeg.startNodeValue.m + deltaDropped

    val dEseg =
      if(prevSeg.endNode == prevSeg.startNode) 0
      else (
        (prevSeg.endNodeValue.deltaE - prevSeg.startNodeValue.deltaE)
          + 2*a*deltaDropped*(prevSeg.endNodeValue.sumDm - prevSeg.startNodeValue.sumDm)
          + (a*deltaDropped+b)*deltaDropped*(prevSeg.endNodeValue.sumD - prevSeg.startNodeValue.sumD))

    val droneWeightBetweenPrevSegAndState = state.weight + droppedWeights(state.currentPoint)
    val dEbtw = (DistanceMatrix(cbls.longToInt(prevSeg.endNode))(cbls.longToInt(state.currentPoint))
      * (a * droneWeightBetweenPrevSegAndState*droneWeightBetweenPrevSegAndState
      + b*droneWeightBetweenPrevSegAndState
      + c))

    DroneState(prevWeight,cbls.longToInt(prevSeg.startNode),dEseg+dEbtw+state.energyForComingRoute)
  }

  /**
    *
    * @param s the current state of the drone
    * @param prevSeg the segment of nodes that we insert before the state
    *                BEWARE: this segment will be flipped
    * @return the new state of the drone at the beginning of the flipped prevSeg
    */
  private def bigStepBackwardFlipped(s:DroneState,prevSeg:FlippedPreComputedSubSequence[QuadrativeDroneAutonomy]):DroneState = {

    val deltaDropped = s.weight + droppedWeights(s.currentPoint) - prevSeg.endNodeValue.revM
    val prevWeight = prevSeg.startNodeValue.revM + deltaDropped

    val dEseg =
      if(prevSeg.endNode == prevSeg.startNode) 0
      else ((prevSeg.endNodeValue.revDeltaE - prevSeg.startNodeValue.revDeltaE)
        + 2*a*deltaDropped*(prevSeg.endNodeValue.revSumDm - prevSeg.startNodeValue.revSumDm)
        + (a*deltaDropped + b)*deltaDropped*(prevSeg.endNodeValue.revSumD - prevSeg.startNodeValue.revSumD))

    val droneWeightBetweenPrevSegAndState = s.weight + droppedWeights(s.currentPoint)
    val dEbtw = (DistanceMatrix(cbls.longToInt(prevSeg.endNode))(cbls.longToInt(s.currentPoint))
      *(a*droneWeightBetweenPrevSegAndState*droneWeightBetweenPrevSegAndState
      + b*droneWeightBetweenPrevSegAndState
      + c))

    DroneState(prevWeight,cbls.longToInt(prevSeg.startNode),dEseg + dEbtw + s.energyForComingRoute)
  }


  /**
    * this method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    * @param vehicle the vehicle where pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param preComputedVals list of type type T associated with the route of the vehicle, numbering is by node value, not by position
    */
  override def performPreCompute(vehicle:Long,
                                 routes:IntSequence,
                                 preComputedVals: Array[QuadrativeDroneAutonomy]) : Unit = {

    val route = extractRouteOfVehicle(cbls.longToInt(vehicle),routes).toArray
    val masses = Array.tabulate(route.length)(e => droneWeight-droppedWeights(cbls.longToInt(vehicle)))
    val revmasses = Array.tabulate(route.length)(e => droneWeight)

    for(i <- route.length-2 to 0 by -1){
      masses(i) = droppedWeights(route(i+1)) + masses(i+1)
      revmasses(route.length-1-i) = droppedWeights(route(route.length-2-i)) + revmasses(route.length-2-i)
    }

    preComputedVals(cbls.longToInt(vehicle)) = QuadrativeDroneAutonomy(cbls.longToInt(vehicle),0,masses(0),0,0,0,revmasses(0),0,0)

    for(i <- 1 to route.length-2){
      val atNode = route(i-1)
      val toNode = route(i)
      val atT = preComputedVals(atNode)
      val m = atT.m
      val distanceAtTo = DistanceMatrix(atNode)(toNode)
      preComputedVals(toNode) = QuadrativeDroneAutonomy(
        toNode,
        atT.deltaE + distanceAtTo * (a*m*m + b*m + c),
        masses(i),
        atT.sumDm + distanceAtTo*m,
        atT.sumD + distanceAtTo,
        0,
        revmasses(i),
        0,
        0)
    }
    for(i <- route.length-3 to 0 by -1){
      val atNode = route(i+1)
      val toNode = route(i)
      val atT = preComputedVals(atNode)
      val toT = preComputedVals(toNode)
      val distanceAtTo = DistanceMatrix(atNode)(toNode)
      val revM = atT.revM
      preComputedVals(toNode) = toT.copy(
        revDeltaE = atT.revDeltaE + distanceAtTo*(a*revM*revM + b*revM + c),
        revSumDm = atT.revSumDm + distanceAtTo*revM,
        revSumD = atT.revSumD + distanceAtTo)
    }
  }


  /**
    * this method is called by the framework to verify that operations are calculated correctly
    *
    * @param vehicle the vehicle where the node list must be extracted
    * @param routes the sequence representing the route of all vehicle
    * @return the energy consumption of the vehicle to travel the road (and come back to the starting point of the vehicle)
    */
  override def computeVehicleValueFromScratch(vehicle :Long,
                                              routes: IntSequence) : EnergyAndWeight = {
    val route = extractRouteOfVehicle(cbls.longToInt(vehicle), routes)
    var state = DroneState(
      droneWeight-droppedWeights(cbls.longToInt(vehicle)),
      cbls.longToInt(vehicle),
      0)
    for (i <- route.size - 2 to 0 by -1){
      state = stepBackward(state,route(i))
    }
    EnergyAndWeight(cbls.longToInt(vehicle),state.weight.toInt,state.energyForComingRoute)
  }

  /**
    * this method must be call before closing the model
    *
    * @param vehicle the vehicle where the node list must be extracted
    * @param routes the sequence representing the route of all vehicle
    * @return the node list that the vehicle runs
    */

  def extractRouteOfVehicle(vehicle: Int,
                            routes: IntSequence) : List[Int] = {
    var reversedRoute = List[Int](vehicle)
    var explorer = routes.explorerAtAnyOccurrence(vehicle).head
    while(explorer.next match{
      case None =>
        // we are at the end of the road of the last vehicle
        false
      case Some(next) =>
        if(next.value < v){
          // we are at the beginning of the next vehicle route
          // we come back to the starting point of the vehicle
          false
        }else{
          // we're still on the road to the vehicle vehicle
          reversedRoute = cbls.longToInt(next.value) :: reversedRoute
          explorer = next
          true
        }
    }){}
    reversedRoute = vehicle :: reversedRoute
    reversedRoute.reverse
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @param preComputedVals the list of type T
    *
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Long,
                                   segments: List[Segment[QuadrativeDroneAutonomy]],
                                   routes: IntSequence,
                                   preComputedVals:Array[QuadrativeDroneAutonomy]): EnergyAndWeight = {
    val state = calcConsumptionAndWeightAtStart(cbls.longToInt(vehicle), segments)
    EnergyAndWeight(cbls.longToInt(vehicle),state.weight.toInt,state.energyForComingRoute)
  }

  /**
    *
    * @param vehicle the vehicle that we are focusing on
    * @param currentSegments the segments that constitute the route
    * @return the state of the drone at the end of the route
    */
  def calcConsumptionAndWeightAtStart(vehicle: Int,
                                      currentSegments:List[Segment[QuadrativeDroneAutonomy]]):DroneState = {
    currentSegments match {
      case Nil => //at the end of the route
        // return to the start, knowing that the drone is empty
        // weight of the empty drone
        DroneState(droneWeight-droppedWeights(vehicle),vehicle,0)
      case head::tail => //not at the end of the route
        val stateAfter = calcConsumptionAndWeightAtStart(vehicle,tail)
        addSegmentReturnWeightAtStartAndConso(head,stateAfter)
    }
  }

  /**
    *
    * @param segment the segment we add before the current state
    * @param s the current state
    * @return the new state of the drone at the beginning of the segment
    */
  def addSegmentReturnWeightAtStartAndConso(segment:Segment[QuadrativeDroneAutonomy],
                                            s:DroneState):DroneState = {

    segment match{
      case PreComputedSubSequence(startNode,
      startNodeValue,endNode,endNodeValue) =>
        bigStepBackward(s,PreComputedSubSequence(startNode,
          startNodeValue,endNode,endNodeValue))

      case FlippedPreComputedSubSequence(startNode,
      startNodeValue, endNode, endNodeValue) =>
        bigStepBackwardFlipped(s,FlippedPreComputedSubSequence(startNode,
          startNodeValue, endNode, endNodeValue))

      case NewNode(node) =>
        stepBackward(s,cbls.longToInt(node))
    }
  }

  /**
    * the framework calls this method to assign the value U to the output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Long, value: EnergyAndWeight): Unit = {
    routeEnergy(cbls.longToInt(vehicle)) := value.energyNeeded
    routeWeight(cbls.longToInt(vehicle)) := value.weight
  }

}
