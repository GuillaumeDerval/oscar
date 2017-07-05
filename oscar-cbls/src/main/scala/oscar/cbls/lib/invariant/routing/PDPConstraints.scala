package oscar.cbls.lib.invariant.routing

import oscar.cbls.business.routing.model.PDP
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{EQ, GE, LE}
import oscar.cbls.lib.invariant.logic.IntITE
import oscar.cbls.lib.invariant.seq.Precedence

/**
  * Created by fg on 5/05/17.
  */
object PDPConstraints {
  def apply(
             pdp: PDP
           ): (ConstraintSystem,ConstraintSystem) ={
    val fastConstraints = new ConstraintSystem(pdp.routes.model)
    val slowConstraints = new ConstraintSystem(pdp.routes.model)

    val pDPConstraints = new PDPConstraints(pdp, fastConstraints, slowConstraints)
    pDPConstraints.addCapacityConstraint()
    pDPConstraints.addTimeWindowConstraints()
    pDPConstraints.addPrecedencesConstraints()
    pDPConstraints.addMaxDetoursConstraints()
    pDPConstraints.addExclusiveCarConstraints()

    (fastConstraints, slowConstraints)
  }
}

class PDPConstraints(pdp: PDP, fastConstraints: ConstraintSystem, slowConstraints: ConstraintSystem){
  import oscar.cbls.modeling.Algebra._

  val routes = pdp.routes
  val n = pdp.n
  val v = pdp.v

  /**
    * Add the precedences constraints.
    * Typically, we want to keep the order of the nodes of each chain
    */
  def addPrecedencesConstraints() {
    val chains = pdp.chains
    val vehicleOfNodes = VehicleOfNodes(pdp.routes,pdp.v)

    def chainToTuple(chain: List[Int], tuples: List[(Int, Int)]): List[(Int, Int)] = {
        if (chain.length <= 1)
          tuples
        else {
          if(v > 1) fastConstraints.add(EQ(vehicleOfNodes(chain.head),vehicleOfNodes(chain.tail.head)))
          chainToTuple(chain.tail, (chain.head, chain.tail.head) :: tuples)
        }
      }

    val chainsPrecedences = List.tabulate(chains.length)(c => chainToTuple(chains(c), List.empty).reverse)
    fastConstraints.add(EQ(0,new Precedence(routes, chainsPrecedences.flatten)))
  }

  /**
    * Given a list of lists of cars, this constraints ensure that for each List[car]
    * only one car will be used.
    */
  def addExclusiveCarConstraints(): Unit ={
    fastConstraints.add(
      EQ(0,
        pdp.exclusiveCarsSubsets
      )
    )
  }

  /**
    * This method adds the maxDetour constraints to the constraints system.
    * A maxDetour constraint is a constraint that says:
    * The actual travel duration between two nodes can't be more than x seconds longer than
    * the shortest travel duration between this two nodes.
    * (If there is some nodes between from and to, the shortest path go through this nodes)
    */
  def addMaxDetoursConstraints() = {
    val arrivalTimes = pdp.arrivalTimes
    val leaveTimes = pdp.leaveTimes

    for(maxDetour <- pdp.maxDetours){
      slowConstraints.post(LE(arrivalTimes(maxDetour._1) - leaveTimes(maxDetour._2._1),maxDetour._2._2))
    }
  }



  def addCapacityConstraint(): Unit ={
    fastConstraints.post(LE(pdp.contentAtNode,pdp.vehiclesMaxCapacities.max))
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

    for(i <- 0 until n){
      if(i < v && deadlines(i) != Int.MaxValue) {
        slowConstraints.post(LE(pdp.arrivalTimesAtEnd(i), deadlines(i)).nameConstraint("end of time for vehicle " + i))
      } else {
        if(deadlines(i) != Int.MaxValue)
          slowConstraints.post(LE(leaveTimes(i), deadlines(i)).nameConstraint("end of time window on node " + i))
        if(maxWaitingDurations(i) != Int.MaxValue)
          slowConstraints.post(GE(arrivalTimes(i), earlylines(i)).nameConstraint("start of time window on node (with duration)" + i))
      }
    }

  }
}
