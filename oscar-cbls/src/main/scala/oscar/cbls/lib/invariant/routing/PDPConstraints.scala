package oscar.cbls.lib.invariant.routing

import oscar.cbls.business.routing.model.PDPv2
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{GE, LE}
import oscar.cbls.lib.invariant.logic.IntITE
import oscar.cbls.lib.invariant.seq.Precedence

/**
  * Created by fg on 5/05/17.
  */
object PDPConstraints {
  def apply(
             pdp: PDPv2,
             additionalPrecedences: List[(Int,Int)] = List.empty,
             maxDetours: List[(Int,Int,Int)] = List.empty,
             maxDetourCalculation:(Int,Int) => Int = (a,b) => a + b
           ): ConstraintSystem ={
    val constraints = new ConstraintSystem(pdp.routes.model)

    val pDPConstraints = new PDPConstraints(constraints, pdp)
    pDPConstraints.addCapacityConstraint()
    pDPConstraints.addTimeWindowConstraints()
    pDPConstraints.addPrecedencesConstraints(additionalPrecedences)
    pDPConstraints.addMaxDetoursConstraints(maxDetours,maxDetourCalculation)

    constraints
  }
}

class PDPConstraints(constraints: ConstraintSystem, pdp: PDPv2) {
  import oscar.cbls.modeling.Algebra._

  val routes = pdp.routes
  val n = pdp.n
  val v = pdp.v

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
    * @param maxDetourCalculation This function define the way we want the maxDetour to be calculate.
    *                             By default, we simply add the maxDetour value to the travel duration between from and to
    */
  def addMaxDetoursConstraints(maxDetours: List[(Int, Int, Int)], maxDetourCalculation:(Int,Int) => Int) = {
    val arrivalTimes = pdp.arrivalTimes
    val leaveTimes = pdp.leaveTimes
    val travelDurationMatrix = pdp.travelDurationMatrix

    for(maxDetour <- maxDetours){
      constraints.add(LE(arrivalTimes(maxDetour._2) - leaveTimes(maxDetour._1),
        maxDetourCalculation(maxDetour._3,
          travelDurationMatrix.getTravelDuration(maxDetour._1, leaveTimes(maxDetour._1).value, maxDetour._2))))
    }
  }

  /**
    * Add the precedences constraints.
    * Typically, we want to keep the order of the nodes of each chain
    * @param additionalPrecedences an additional list of precedences we want to add
    */
  def addPrecedencesConstraints(additionalPrecedences:List[(Int,Int)]): Unit ={
    val chains = pdp.chains

    def chainToTuple(chain: List[Int], tuples:List[(Int,Int)]): List[(Int,Int)] ={
      if(chain.length <= 1)
        tuples
      else
        chainToTuple(chain.tail, (chain.head,chain.tail.head) :: tuples)
    }

    val chainsPrecedences = List.tabulate(chains.length)(c => chainToTuple(chains(c).toList, List.empty).reverse)
    new Precedence(routes, chainsPrecedences.flatten ++ additionalPrecedences)
  }

  def addCapacityConstraint(): Unit ={
    val vehiclesMaxCapacities = pdp.vehiclesMaxCapacities
    val contentsFlow = pdp.contentsFlow
    val vehiclesMaxCapacity:Int = vehiclesMaxCapacities.max
    val contentAtVehicleStart = Array.tabulate(v)(i => vehiclesMaxCapacity-vehiclesMaxCapacities(i))
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
    */
  def addTimeWindowConstraints()={

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
