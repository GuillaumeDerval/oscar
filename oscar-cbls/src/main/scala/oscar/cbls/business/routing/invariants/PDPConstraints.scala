package oscar.cbls.business.routing.invariants

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.capa.{ForwardCumulativeConstraintOnVehicle, ForwardCumulativeIntegerIntegerDimensionOnVehicle}
import oscar.cbls.business.routing.model.extensions._
import oscar.cbls.lib.constraint.{EQ, GE, LE}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.seq.Precedence
import oscar.cbls.lib.invariant.set.IncludedSubsets

/**
  * Created by fg on 5/05/17.
  */
object PDPConstraints {
  def apply(
             vrp: VRP,
             capacityInvariant: Option[ForwardCumulativeConstraintOnVehicle] = None,
             timeWindow: Option[TimeWindow] = None,
             timeWindowInvariant: Option[ForwardCumulativeIntegerIntegerDimensionOnVehicle] = None,
             maxTravelDurations: Option[Map[(Int,Int),Int]] = None,
             precedences: Option[Precedence] = None,
             carExclusivitiesSubsets: Option[IncludedSubsets] = None,
             nodeVehicleObligations: Option[Array[CBLSIntVar]] = None
           ): (ConstraintSystem,ConstraintSystem) ={
    require((timeWindow.isDefined && timeWindowInvariant.isDefined) || (timeWindow.isEmpty && timeWindowInvariant.isEmpty),
    "You can either define the timeWindow and the timeWindowInvariant or you defined neither of them")
    require((maxTravelDurations.isDefined && timeWindowInvariant.isDefined) || maxTravelDurations.isEmpty,
    "If you use maxTravelDurations constraints, you must defined a timeWindowInvariant")

    val fastConstraints = new ConstraintSystem(vrp.routes.model)
    val slowConstraints = new ConstraintSystem(vrp.routes.model)

    val pDPConstraints = new PDPConstraints(vrp, fastConstraints, slowConstraints)
    if(capacityInvariant.isDefined) pDPConstraints.addCapacityConstraint(capacityInvariant.get)
    if(timeWindowInvariant.isDefined && timeWindow.isDefined) pDPConstraints.addTimeWindowConstraints(timeWindow.get,timeWindowInvariant.get)
    if(precedences.isDefined) pDPConstraints.addPrecedencesConstraints(precedences.get)
    if(maxTravelDurations.isDefined) pDPConstraints.addMaxTravelDurationConstraint(timeWindowInvariant.get,maxTravelDurations.get)
    if(carExclusivitiesSubsets.isDefined) pDPConstraints.addExclusiveCarConstraints(carExclusivitiesSubsets.get)
    if(nodeVehicleObligations.isDefined) pDPConstraints.addVehiclesObligations(nodeVehicleObligations.get)

    (fastConstraints, slowConstraints)
  }
}

class PDPConstraints(vrp: VRP, fastConstraints: ConstraintSystem, slowConstraints: ConstraintSystem){

  private val n = vrp.n
  private val v = vrp.v

  /**
    * Add the precedences constraints.
    * Typically, we want to keep the order of the nodes of each chain
    */
  private def addPrecedencesConstraints(precedences: Precedence) {
    val vehicleOfNodes = vrp.vehicleOfNode
    for(start <- precedences.nodesStartingAPrecedence)
      fastConstraints.add(EQ(vehicleOfNodes(start),vehicleOfNodes(precedences.nodesEndingAPrecedenceStartedAt(start).head)))
    fastConstraints.add(EQ(0,precedences))
  }

  /**
    * Given a list of lists of cars, this constraints ensure that for each List[car]
    * only one car will be used.
    */
  private def addExclusiveCarConstraints(exclusivesCarSubset: IncludedSubsets): Unit ={
    fastConstraints.add(EQ(0,exclusivesCarSubset))
  }

  /**
    * This method adds the maxDetour constraints to the constraints system.
    * A maxDetour constraint is a constraint that says:
    * The actual travel duration between two nodes can't be more than x seconds longer than
    * the shortest travel duration between this two nodes.
    * (If there is some nodes between from and to, the shortest path go through this nodes)
    * @param timeWindowInvariant The timeWindowInvariant to which the maxTravelDurations are attached
    * @param maxTravelDurations A map : ((from,to) => maxTravelDuration)
    */
  private def addMaxTravelDurationConstraint(timeWindowInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle, maxTravelDurations: Map[(Int,Int),Int]) = {
    val arrivalTimes = timeWindowInvariant.content1AtNode
    val leaveTimes = timeWindowInvariant.content2AtNode

    for(maxDetour <- maxTravelDurations){
      slowConstraints.post(LE(arrivalTimes(maxDetour._1._2) - leaveTimes(maxDetour._1._1),maxDetour._2))
    }
  }



  private def addCapacityConstraint(capacityInvariant: ForwardCumulativeConstraintOnVehicle): Unit ={
    fastConstraints.post(EQ(capacityInvariant.violation,0))
  }

  /**
    * This method is used to set timeWindow related constraints :
    *   1° : Maximum arrival time at depot (for vehicle)
    *   2° : Maximum arrival time at node
    *   3° : Maximum departure time at node (using maxWaitingTime)
    */
  private def addTimeWindowConstraints(timeWindow: TimeWindow, timeWindowInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle)={
    val earlylines = timeWindow.earlylines
    val deadlines = timeWindow.deadlines
    val maxWaitingDurations = timeWindow.maxWaitingDurations
    val arrivalTimes = timeWindowInvariant.content1AtNode
    val leaveTimes = timeWindowInvariant.content2AtNode
    val arrivalTimesAtEnd = timeWindowInvariant.content1AtEnd

    for(i <- 0 until n){
      if(i < v && deadlines(i) != Int.MaxValue) {
        slowConstraints.post(LE(arrivalTimesAtEnd(i), deadlines(i)).nameConstraint("end of time for vehicle " + i))
      } else {
        if(deadlines(i) != Int.MaxValue)
          slowConstraints.post(LE(leaveTimes(i), deadlines(i)).nameConstraint("end of time window on node " + i))
        if(maxWaitingDurations(i) != Int.MaxValue)
          slowConstraints.post(GE(arrivalTimes(i), earlylines(i)).nameConstraint("start of time window on node (with duration)" + i))
      }
    }

  }

  private def addVehiclesObligations(obligations: Array[CBLSIntVar])={
    if(v > 1)
      fastConstraints.post(EQ(0, Sum(obligations)))
  }
}
