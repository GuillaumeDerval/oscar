package oscar.cbls.business.routing.invariants

import oscar.cbls._
import oscar.cbls.core._

/**
 * Created by rdl on 11-09-17.
 */
trait RoutingInvriants {

  /**
   * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
   *
   * This invariant relies on the vehicle model assumption:
   * there are v vehicles
   * They are supposed to start from point of values 0 to v-1
   * These values must always be present in the sequence in increasing order
   * they cannot be included within a moved segment
   *
   * @param routes the routes of all the vehicles
   * @param n
   * @param v the number of vehicles in the model
   * @param perVehicle true if you request the distance per vehicle (slower)
   *                   false if you want the total distance among all vehicles
   * @param distanceMatrix the matrix of distance
   * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
   * @param precomputeFW performs forward pre-computation, only useful if multiple vehicle and per vehicle distance or asymetric matrix
   * @param precomputeBW performs backward pre-computation, only useful if assymetric matrix
   * @return if the distance is PerVehicle, it is an array that maps each vehicle to its drive distance
   *         if the distance is global, it is a array with a single variable that is equal to the sum of all drive distance
   */
  def constantRoutingDistance(routes : ChangingSeqValue,
                              n : Int,
                              v : Int,
                              perVehicle : Boolean,
                              distanceMatrix : Array[Array[Int]],
                              distanceIsSymmetric : Boolean,
                              precomputeFW : Boolean = false,
                              precomputeBW : Boolean = false) : Array[CBLSIntVar] =
    ConstantRoutingDistance(routes,
      n,
      v,
      perVehicle,
      distanceMatrix,
      distanceIsSymmetric,
      precomputeFW,
      precomputeBW)

  /**
   * maintains the set of vehicle that have at least one point to visit (beyond their start point)
   * this invariant relies on the routing convension.
   * @param routes the routes
   * @param v the number of vehicle
   */
  def movingVehicles(routes : ChangingSeqValue, v : Int) =
    MovingVehicles(routes, v)

  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of obligation node->set of vehicles (it must reach one vehicle among any of the given set, or be non-routed)
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * we consider that a node that is not routed does not violate the obligation constraint
   * @param routes
   * @param v the number of vehicles
   * @param n the number of nodes
   * @param nodeVehicleObligation the obligation that we are monitoring
   * @return an array telling the violation per vehicle
   * @note this is a preliminary naive version of the constraint. a faster one is to be developed!
   */
  def nodeVehicleObligation(routes : ChangingSeqValue, v : Int, n : Int, nodeVehicleObligation : Map[Int, Set[Int]]) : Array[CBLSIntVar] =
    NodeVehicleObligation(routes, v, n, nodeVehicleObligation)

  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of restrictions node<->vehicle,
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * @param routes
   * @param v
   * @param nodeVehicleRestrictions the restrictions that we are monitoring
   * @return an array telling the violation per vehicle
   */
  def nodeVehicleRestrictions(routes : ChangingSeqValue, v : Int, nodeVehicleRestrictions : Iterable[(Int, Int)]) =
    NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions)






}