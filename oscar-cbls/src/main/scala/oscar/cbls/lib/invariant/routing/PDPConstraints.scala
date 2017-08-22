package oscar.cbls.lib.invariant.routing

import oscar.cbls.business.routing.model.PDP
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{EQ, GE, LE}
import oscar.cbls.lib.invariant.logic.IntITE
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.seq.Precedence

/**
  * Created by fg on 5/05/17.
  */
object PDPConstraints {
  def apply(
             pdp: PDP,
             restrictions: List[(Int,Int)] = List.empty
           ): (ConstraintSystem,ConstraintSystem) ={
    val fastConstraints = new ConstraintSystem(pdp.routes.model)
    val slowConstraints = new ConstraintSystem(pdp.routes.model)

    val pDPConstraints = new PDPConstraints(pdp, fastConstraints, slowConstraints)
    pDPConstraints.addCapacityConstraint()
    pDPConstraints.addTimeWindowConstraints()
    pDPConstraints.addPrecedencesConstraints()
    pDPConstraints.addMaxDetoursConstraints()
    pDPConstraints.addExclusiveCarConstraints()
    pDPConstraints.addVehiclesRestrictions(restrictions)

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
    fastConstraints.post(EQ(pdp.contentAtNode,0))
  }

  /**
    * This method is used to set timeWindow related constraints :
    *   1° : Maximum arrival time at depot (for vehicle)
    *   2° : Maximum arrival time at node
    *   3° : Maximum departure time at node (using maxWaitingTime)
    */
  def addTimeWindowConstraints()={

    for(i <- 0 until n){
      if(i < v && pdp.deadlines(i) != Int.MaxValue) {
        slowConstraints.post(LE(pdp.arrivalTimesAtEnd(i), pdp.deadlines(i)).nameConstraint("end of time for vehicle " + i))
      } else {
        if(pdp.deadlines(i) != Int.MaxValue)
          slowConstraints.post(LE(pdp.leaveTimes(i), pdp.deadlines(i)).nameConstraint("end of time window on node " + i))
        if(pdp.maxWaitingDurations(i) != Int.MaxValue)
          slowConstraints.post(GE(pdp.arrivalTimes(i), pdp.earlylines(i)).nameConstraint("start of time window on node (with duration)" + i))
      }
    }

  }

  def addVehiclesRestrictions(restrictions: List[(Int,Int)])={
    if(v > 1)
      fastConstraints.post(EQ(0, Sum(NodeVehicleRestrictions(routes,v,restrictions))))
  }
}
