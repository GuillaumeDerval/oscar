package oscar.cbls.lib.invariant.routing

import oscar.cbls.business.routing.legacy.model.TTFMatrix
import oscar.cbls.business.routing.model.PDPv2
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, IntValue}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{GE, LE}
import oscar.cbls.lib.invariant.logic.IntITE
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.seq.Precedence

/**
  * Created by fg on 5/05/17.
  */
object PDPConstraints {
  def apply(
             pdp: PDPv2,
             maxDetours: List[(Int,Int,Int)] = List.empty,
             multiple: Boolean = false
           ): ConstraintSystem ={
    val constrains = new ConstraintSystem(pdp.routes.model)

    val pDPConstraints = new PDPConstraints(constrains, pdp.routes)
    pDPConstraints.addCapacityConstraint(pdp.n,pdp.v,pdp.vehicleMaxCapacity,pdp.contentsFlow)
    pDPConstraints.addTimeWindowConstraints(pdp)
    pDPConstraints.addPrecedencesConstraints(pdp.chains)
    pDPConstraints.addMaxDetoursConstraints(maxDetours,pdp,multiple)

    constrains
  }
}

class PDPConstraints(constraints: ConstraintSystem, routes: ChangingSeqValue) {
  import oscar.cbls.modeling.Algebra._

  /**
    * This method adds the maxDetour constraints to the constraints system.
    * A maxDetour constraint is a constraint that says:
    * The actual travel duration between two nodes can't be more than x seconds longer than
    * the shortest travel duration between this two nodes.
    * (If there is some nodes between from and to, the shortest path go through this nodes)
    * @param maxDetours a tuple of Int where :
    *                   1° from node
    *                   2° to node
    *                   3° maximum detour (x)
    * @param pdp The Pickup and Delivery Problem containing all the information needed
    * @param multiple If true, we multiply the shortest distance by a certain value instead of adding seconds to it
    */
  def addMaxDetoursConstraints(maxDetours: List[(Int, Int, Int)], pdp: PDPv2, multiple:Boolean) = {
    val arrivalTimes = pdp.arrivalTimes
    val leaveTimes = pdp.leaveTimes
    val travelDurationMatrix = pdp.travelDurationMatrix

    for(maxDetour <- maxDetours){
      if(multiple)
        constraints.add(LE(arrivalTimes(maxDetour._2) - leaveTimes(maxDetour._1), maxDetour._3+travelDurationMatrix.getTravelDuration(maxDetour._1, leaveTimes(maxDetour._1).value, maxDetour._2)))
      else
        constraints.add(LE(arrivalTimes(maxDetour._2) - leaveTimes(maxDetour._1), maxDetour._3+travelDurationMatrix.getTravelDuration(maxDetour._1, leaveTimes(maxDetour._1).value, maxDetour._2)))

    }
  }

  /**
    * Add the precedences constraints.
    * Typically, we want to keep the order of the nodes of each chain
    * @param chains The chains
    */
  def addPrecedencesConstraints(chains:Array[Array[Int]]): Unit ={

    def chainToTuple(chain: Array[Int]): List[(Int,Int)] ={
      if(chain.tail.isEmpty)
        List.empty
      else
        List((chain.head,chain.tail.head)) ++ chainToTuple(chain.tail)
    }

    val test = List.tabulate(chains.length)(c => chainToTuple(chains(c)))
    new Precedence(routes, test.flatten)
  }

  def addCapacityConstraint(n:Int, v:Int, vehicleMaxCapacity:Array[Int], contentsFlow:Array[Int]): Unit ={
    val vehiclesMaxCapacity:Int = vehicleMaxCapacity.sortBy(x => x).last
    val contentAtVehicleStart = Array.tabulate(v)(i => vehiclesMaxCapacity-vehicleMaxCapacity(i))
    ForwardCumulativeConstraintOnVehicle(
      routes,
      n,
      v,
      (from,to,fromContent) => fromContent + contentsFlow(to),
      vehiclesMaxCapacity,
      contentAtVehicleStart,
      4,
      4,
      "VehicleMaxCapacity"
    )
  }

  /**
    * This method is used to set timeWindow related constraints :
    *   1° : Maximum arrival time at depot (for vehicle)
    *   2° : Maximum arrival time at node
    *   3° : Maximum departure time at node (using maxWaitingTime)
    * @param pdp The Pickup and Delivery Problem containing all the information needed
    */
  def addTimeWindowConstraints(pdp: PDPv2)={

    val n = pdp.n
    val v = pdp.v
    val earlylines = pdp.earlylines
    val maxWaitingDurations = pdp.maxWaitingDurations
    val deadlines = pdp.deadlines
    val arrivalTimes = pdp.arrivalTimes
    val leaveTimes = pdp.leaveTimes
    val waitingDurations = pdp.waitingDurations

    for(i <- 0 until n){
      if(i < v && deadlines(i) != Int.MaxValue) {
        constraints.post(LE(arrivalTimes(i), deadlines(i)).nameConstraint("end of time for vehicle " + i))
      } else {
        if(deadlines(i) != Int.MaxValue)
          constraints.post(LE(IntITE(pdp.next(i), 0, leaveTimes(i), n-1), deadlines(i)).nameConstraint("end of time window on node " + i))
        if(maxWaitingDurations(i) != Int.MaxValue)
          constraints.post(GE(arrivalTimes(i), earlylines(i) - waitingDurations(i)).nameConstraint("start of time window on node (with duration)" + i))
      }
    }

  }
}
