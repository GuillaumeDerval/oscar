package oscar.cbls.invariants.lib.routing

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

object ConstantRoutingDistance {

  /**
   *
   * @param routes
   * @param v
   * @param perVehicle
   * @param distanceMatrix
   * @param distanceIsSymmetric
   * @param precomputeFW performs forward pre-computation, only useful if multiple vehicle and per vehicle disatnce or asymetric matrix
   * @param precomputeBW performs backward pre-computation, only useful if assymetric matrix
   * @return
   */
  def apply(routes : ChangingSeqValue,
            v : Int,
            perVehicle:Boolean,
            distanceMatrix : Array[Array[Int]],
            distanceIsSymmetric : Boolean,
            precomputeFW:Boolean = false,
            precomputeBW:Boolean = false):Array[CBLSIntVar] = {

    val distance:Array[CBLSIntVar] =
      if(perVehicle) Array.tabulate(v)(v => CBLSIntVar(routes.model,name="distanceOfVehicle" + v))
      else Array.fill(1)(CBLSIntVar(routes.model,name="overallDistance"))

    if(precomputeFW || precomputeBW){
      new ConstantRoutingDistancePrecompute(routes,
        v,
        distanceMatrix,
        distance,
        distanceIsSymmetric,
        precomputeFW,precomputeBW)
    }else{
      new ConstantRoutingDistance(routes,
        v,
        distanceMatrix,
        distance,
        distanceIsSymmetric)
    }
    distance
  }

  def isDistanceSymmetric(distanceMatrix : Array[Array[Int]]):Boolean = {
    val n = distanceMatrix.length
    var i = 0
    while(i < n){
      var j = 0
      while(j <= i){
        if (distanceMatrix(i)(j) != distanceMatrix(j)(i))
          return false
        j += 1
      }
      i += 1
    }
    true
  }
}

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in which case it is the total distance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
 *
 * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class ConstantRoutingDistance(routes:ChangingSeqValue,
                              v:Int,
                              distanceMatrix:Array[Array[Int]],
                              distance:Array[CBLSIntVar],
                              distanceIsSymmetric:Boolean)
  extends Invariant() with SeqNotificationTarget{

  val perVehicle:Boolean = distance.length >1
  require(distance.length == 1 || distance.length == v)

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distance) i.setDefiningInvariant(this)

  //TODO: handle inactive checkpoints
  private val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  protected var checkpoint = routes.value

  //TODO: use magic array here.
  private val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  protected var touchedRoutesSinceCheckpointList:QList[Int] = null

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes,false)) {
      for(v <- 0 until this.v) recordTouchedVehicle(v)
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate,skipNewCheckpoints:Boolean):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isStarMode:Boolean) =>
        //TODO: manage levels!

        if(!digestUpdates(prev,true)){
          affect(computeValueFromScratch(changes.newValue))
        }
        saveCurrentCheckpoint(changes.newValue)
        true
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        require (checkpoint quickEquals this.checkpoint)
        restoreCheckpoint()
        true
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev,skipNewCheckpoints)) return false
        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).head

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1 //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop
        }

        val oldDistance = distanceMatrix(oldPrev)(oldSucc)
        val newDistance = distanceMatrix(oldPrev)(value) + distanceMatrix(value)(oldSucc)
        val nodeCost = distanceMatrix(value)(value)


        if(perVehicle) {
          val vehicle = vehicleSearcher(newSeq, pos)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance + nodeCost - oldDistance)
        }else{
          distance(0) :+= (newDistance + nodeCost - oldDistance)
        }
        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev,skipNewCheckpoints)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrix(fromValue)(oldSuccToValue)

          //for simple flip, there is no node cost to consider
          if(perVehicle) {
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val deltaDistance = if(distanceIsSymmetric) 0 else {
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)
            }
            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }else{//not per vehicle
          val deltaDistance = if(distanceIsSymmetric) 0 else {
              val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)
            }
            distance(0) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }
          true
        }else {
          //actually moving, not simple flip
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1
            case Some(value) => if (value < v) value - 1 else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrix(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrix(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrix(if(flip) fromValue else toValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrix(oldPrevFromValue)(oldSuccToValue)

          if(!perVehicle){
            //not per vehicle, so no node cost to consider
            val deltaDistance = if(distanceIsSymmetric || !flip) 0 //no delta on distance
            else { //there is a flip and distance is asymmetric
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)

            }
            distance(0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          }else {
            //per vehicle, there might be some node cost to consider
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
            assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle, so we do not consider node cost here

              val (deltaDistance) = if(distanceIsSymmetric || !flip) 0 else {
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded, toValue,
                  fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                  - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

            } else {
              //moving a segment to another vehicle, and per vehicle required.

              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
              val oldCostInSegment = computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded,toValue)
              val newCostInSegment = if(distanceIsSymmetric || !flip) oldCostInSegment else{
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded,toValue,
                  fromIncluded, fromValue)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldCostInSegment))

              recordTouchedVehicle(targetVehicleOfMove)
              distance(targetVehicleOfMove) :+= (
                newHopBeforeMovedSegment + newCostInSegment + newHopAfterMovedSegment - oldHopAfterAfter)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev,skipNewCheckpoints)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).head //vehicles are never deleted
        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)
        val newDistance = distanceMatrix(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrix(oldPrevValue)(removedValue)
        val oldDistanceAfter = distanceMatrix(removedValue)(oldSuccValue)
        val nodeCost = distanceMatrix(removedValue)(removedValue)

        if(perVehicle){
          val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }else{
          distance(0) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }
        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  protected def saveCurrentCheckpoint(s:IntSequence){
    checkpoint = s
    if(perVehicle) {
      while (touchedRoutesSinceCheckpointList != null) {
        touchedRoutesSinceCheckpointArray(touchedRoutesSinceCheckpointList.head) = false
        touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
      }
    }else{
      savedValues(0) = distance(0).newValue
    }

    //TODO: find stronger condition
    if(v > 1) vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint,v)
  }

  private def restoreCheckpoint(){
    if(perVehicle) {
      while (touchedRoutesSinceCheckpointList != null) {
        val v = touchedRoutesSinceCheckpointList.head
        distance(v) := savedValues(v)
        touchedRoutesSinceCheckpointArray(v) = false
        touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
      }
    }else{
      distance(0) := savedValues(0)
    }
  }

  private def recordTouchedVehicle(v:Int){
    if(perVehicle){
      if(checkpoint!= null && !touchedRoutesSinceCheckpointArray(v)){
        savedValues(v) = distance(v).newValue
        touchedRoutesSinceCheckpointArray(v) = true
        touchedRoutesSinceCheckpointList = QList(v,touchedRoutesSinceCheckpointList)
      }
    }
  }

  private def affect(value:Array[Int]){
    var currentV = distance.length
    while(currentV >0){
      currentV -= 1
      distance(currentV) := value(currentV)
    }
  }

  // labeled forward and backward nodes with their cumulated distance
  // use invalidation per vehicle in case more than one move is performed
  // just one thing: backtrack is only performed through checkpoint; star mode will lead to recomputation of the vehicles from scratch
  //datastruct for checkpoint: forward et bw labeling per vehicle. labeling: node -> (forward,backward) in a redBlack
  protected def computeValueBetween(s:IntSequence, vehicle:Int, fromPosIncluded:Int, fromValueIncluded:Int, toPosIncluded:Int, toValueIncluded:Int):Int = {
    if(fromPosIncluded <= toPosIncluded) {
      var e = s.explorerAtPosition(fromPosIncluded).head
      var toReturn = distanceMatrix(e.value)(e.value)

      while (e.position < toPosIncluded) {
        val nextPos = e.next.head
        toReturn += distanceMatrix(e.value)(nextPos.value) + distanceMatrix(nextPos.value)(nextPos.value)
        e = nextPos
      }
      toReturn
    }else{
      var e = s.explorerAtPosition(fromPosIncluded).head
      var toReturn = distanceMatrix(e.value)(e.value)

      while (e.position > toPosIncluded) {
        val prevPos = e.prev.head
        toReturn += distanceMatrix(e.value)(prevPos.value) + distanceMatrix(prevPos.value)(prevPos.value)
        e = prevPos
      }
      toReturn
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[Int] = {
    val toReturn = Array.tabulate(v)(v => distanceMatrix(v)(v))
    val it = s.iterator

    var prevNode:Int = it.next()
    var currentVehicle:Int = prevNode

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle (cost of vehicle node already added)
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(node) +  distanceMatrix(node)(node)
      }
      prevNode = node
    }
    //for the last vehicle, the finishing operation in the loop will not be executed, so we have to add one here
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)

    if(perVehicle) toReturn
    else Array.fill(1)(toReturn.sum)
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check(!distanceIsSymmetric || ConstantRoutingDistance.isDistanceSymmetric(distanceMatrix),Some("distance matrix should be symmetric if invariant told so"))

    if(perVehicle){
      val values = computeValueFromScratch(routes.value)
      for (vehicle <- 0 to v-1){
        c.check(distance(vehicle).value == values(vehicle),Some("distance("+vehicle+").value=" + distance(vehicle).newValue + " should== computeValueFromScratch(routes.value)(0)" + values(vehicle)))
      }

      if(checkpoint != null) {
        val values = computeValueFromScratch(checkpoint)
        for (vehicle <- 0 to v - 1) {
          if(touchedRoutesSinceCheckpointArray(vehicle))
            c.check(savedValues(vehicle) == values(vehicle))
        }
      }

    }else{
      c.check(distance(0).newValue == computeValueFromScratch(routes.value)(0),Some("distance(0).value=" + distance(0).newValue + " should== computeValueFromScratch(routes.value)(0)" + computeValueFromScratch(routes.value)(0)))
      if(checkpoint != null){
        c.check(savedValues(0) == computeValueFromScratch(checkpoint)(0))
      }
    }
  }
}


/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in which case it is the total distance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
 *
 * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class ConstantRoutingDistancePrecompute(routes:ChangingSeqValue,
                                        v:Int,
                                        distanceMatrix:Array[Array[Int]],
                                        distance:Array[CBLSIntVar],
                                        distanceIsSymmetric:Boolean,
                                        precomputeFW:Boolean,
                                        precomputeBW:Boolean)
  extends ConstantRoutingDistance(routes:ChangingSeqValue,
    v:Int,
    distanceMatrix:Array[Array[Int]],
    distance:Array[CBLSIntVar],
    distanceIsSymmetric:Boolean) {

  val n = routes.maxValue + 1

  //TODO: for stacked updates, we will use someting like magic arrays here, indiced by level of checkpoints, with constant limit on the stack, so that all arrays can be allocated at startups
  val isFWPrecomputeUpToDate:Array[Boolean] = Array.fill(v)(false)
  val isBWPrecomputeUpToDate:Array[Boolean] = Array.fill(v)(false)
  private val precomputedForwardCumulatedCostAtCheckpoint : Array[Int] = if(precomputeFW) Array.fill(n)(0) else null
  private val precomputedBackwardCumulatedCostAtCheckpont : Array[Int] = if(precomputeBW) Array.fill(n)(0) else null


  /* forward pour commencer
  * node => distance
  *
  * is vehicle pre-computed: array v->bool
  * is vehicle changed:
  * push --> reset array at this level, all out of date
  *
  *
  *
  * */

  def computePrecomputedForwardCumulatedCostAtCheckpoint(vehicle : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).head.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => false //we finished the last vehicle
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1){
          precomputedForwardCumulatedCostAtCheckpoint(value) =
            precomputedForwardCumulatedCostAtCheckpoint(prevValue) +
              distanceMatrix(prevValue)(value) +
              distanceMatrix(prevValue)(prevValue)
          explorerOPt = explorer.next
          prevValue = value
          true
        } else false
    }){}
  }

  def computePrecomputedBackwardCumulatedCostAtCheckpoint(vehicle : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).head.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => false
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1) {
          precomputedBackwardCumulatedCostAtCheckpont(value) =
            precomputedBackwardCumulatedCostAtCheckpont(prevValue) +
              distanceMatrix(value)(prevValue) +
              distanceMatrix(value)(value)
          explorerOPt = explorer.next
          prevValue = value
          true
        } else false
    }){}
  }

  override protected def computeValueBetween(s : IntSequence, vehicle:Int,
                                             fromPosIncluded : Int, fromValueIncluded:Int,
                                             toPosIncluded : Int, toValueIncluded:Int) : Int = {
    val atCheckpoint = s quickEquals checkpoint
    val forwardRequired = fromPosIncluded < toPosIncluded

    if(fromPosIncluded == toPosIncluded) {
      distanceMatrix(fromValueIncluded)(fromValueIncluded)
    } else if(atCheckpoint && precomputeFW && forwardRequired){
      //Forward
      doFWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded)
        + distanceMatrix(fromValueIncluded)(fromValueIncluded)
        - precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
      toReturn
    }else if(atCheckpoint && precomputeBW && !forwardRequired) {
      //backward
      doBWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(fromValueIncluded)
        + distanceMatrix(toValueIncluded)(toValueIncluded)
        - precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded, toPosIncluded, toValueIncluded))
      toReturn
    }else if(atCheckpoint && (precomputeFW || precomputeBW) && distanceIsSymmetric){
      //gt the other distance from the available pre-compute
      if(precomputeFW){
        //getting a BW distance from a FW precompute
        doFWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded)
          + distanceMatrix(toValueIncluded)(toValueIncluded)
          - precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded))
        assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
        toReturn
      }else{
        //getting a FW distance from a BW precompute
        doBWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded)
          + distanceMatrix(fromValueIncluded)(fromValueIncluded)
          - precomputedBackwardCumulatedCostAtCheckpont(fromValueIncluded))
        assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded, toPosIncluded, toValueIncluded))
        toReturn
      }
    }else{
      super.computeValueBetween(s, vehicle:Int, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded)
    }
  }

  def doFWPrecomputeForVehicle(vehicle:Int){
    if(isFWPrecomputeUpToDate(vehicle)) return
    computePrecomputedForwardCumulatedCostAtCheckpoint(vehicle,checkpoint)
    isFWPrecomputeUpToDate(vehicle) = true
  }

  def doBWPrecomputeForVehicle(vehicle:Int){
    if(isBWPrecomputeUpToDate(vehicle)) return
    computePrecomputedBackwardCumulatedCostAtCheckpoint(vehicle,checkpoint)
    isBWPrecomputeUpToDate(vehicle) = true
  }

  override protected def saveCurrentCheckpoint(s : IntSequence) : Unit = {
    //records what has changed since last checkpoint, to update only these pre-computations, lazily

    var routesToPrecompute = touchedRoutesSinceCheckpointList
    while(routesToPrecompute != null){
      val currentRoute=routesToPrecompute.head
      routesToPrecompute = routesToPrecompute.tail
      isFWPrecomputeUpToDate(currentRoute) = false
      isBWPrecomputeUpToDate(currentRoute) = false
    }

    super.saveCurrentCheckpoint(s)
  }
}
