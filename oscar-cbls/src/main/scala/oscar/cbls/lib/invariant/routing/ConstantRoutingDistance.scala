package oscar.cbls.lib.invariant.routing

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
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.routing.convention.RoutingConventionMethods

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
            n:Int,
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
        n,
        v,
        distanceMatrix,
        distance,
        distanceIsSymmetric,
        precomputeFW,precomputeBW)
    }else{
      new ConstantRoutingDistance(routes,
        n,
        v,
        distanceMatrix,
        distance,
        distanceIsSymmetric)
    }
    distance
  }

  def isDistanceSymmetricArray(distanceMatrix : Array[Array[Int]]):Boolean = {
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

  def isDistanceSymmetric(distanceMatrix : Int => Int => Int, n:Int):Boolean = {
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
                              n:Int,
                              v:Int,
                              distanceMatrix:Array[Array[Int]],
                              distance:Array[CBLSIntVar],
                              distanceIsSymmetric:Boolean,
                              nodeToMatrixID:Int=>Int = x => x)
  extends Invariant() with SeqNotificationTarget{

  protected def distanceMatrixOnNode(from:Int)(to:Int):Int = {
    distanceMatrix(nodeToMatrixID(from))(nodeToMatrixID(to))
  }

  val perVehicle:Boolean = distance.length >1
  require(distance.length == 1 || distance.length == v)

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distance) i.setDefiningInvariant(this)

  //TODO: handle inactive checkpoints
  private val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  protected var checkpoint = routes.value

  protected[this] val isVehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(false)
  protected var changedVehiclesSinceCheckpoint:QList[Int] = null

  //only one level of stack for checkpoint here.

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes)) {
      for(v <- 0 until this.v) recordTouchedVehicle(v)
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        //we only consider level 0; other are not managed.
        if(checkpointLevel == 0) {

          if (!digestUpdates(prev)) {
            affect(computeValueFromScratch(changes.newValue))
          }
          saveCurrentCheckpoint(changes.newValue)
          true
        }else{
          //ignore it altogether
          digestUpdates(prev)
        }

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpoint)
          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false

        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).get

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1 //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop
        }

        val oldDistance = distanceMatrixOnNode(oldPrev)(oldSucc)
        val newDistance = distanceMatrixOnNode(oldPrev)(value) + distanceMatrixOnNode(value)(oldSucc)
        val nodeCost = distanceMatrixOnNode(value)(value)

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
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(fromValue)(oldSuccToValue)

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
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1
            case Some(value) => if (value < v) value - 1 else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrixOnNode(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrixOnNode(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(if(flip) fromValue else toValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrixOnNode(oldPrevFromValue)(oldSuccToValue)

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
        if(!digestUpdates(prev)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).get //vehicles are never deleted
      val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)
        val newDistance = distanceMatrixOnNode(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrixOnNode(oldPrevValue)(removedValue)
        val oldDistanceAfter = distanceMatrixOnNode(removedValue)(oldSuccValue)
        val nodeCost = distanceMatrixOnNode(removedValue)(removedValue)

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

  /**
   * engages the saving of output values at this checkpoint.
   * you also must call  recordTouchedVehicle(v:Int) for this saving to be effective.
   * @param s
   */
  protected def saveCurrentCheckpoint(s:IntSequence){
    checkpoint = s
    if(perVehicle) {
      while (changedVehiclesSinceCheckpoint != null) {
        isVehicleChangedSinceCheckpoint(changedVehiclesSinceCheckpoint.head) = false
        changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
      }
    }else{
      savedValues(0) = distance(0).newValue
    }

    //TODO: find stronger condition
    if(v > 1) vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint,v)
  }

  private def restoreCheckpoint(){
    if(perVehicle) {
      while (changedVehiclesSinceCheckpoint != null) {
        val v = changedVehiclesSinceCheckpoint.head
        distance(v) := savedValues(v)
        isVehicleChangedSinceCheckpoint(v) = false
        changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
      }
    }else{
      distance(0) := savedValues(0)
    }
  }

  private def recordTouchedVehicle(v:Int){
    if(perVehicle){
      if(checkpoint!= null && !isVehicleChangedSinceCheckpoint(v)){
        savedValues(v) = distance(v).newValue
        isVehicleChangedSinceCheckpoint(v) = true
        changedVehiclesSinceCheckpoint = QList(v,changedVehiclesSinceCheckpoint)
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
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position < toPosIncluded) {
        val nextPos = e.next.get
        toReturn += distanceMatrixOnNode(e.value)(nextPos.value) + distanceMatrixOnNode(nextPos.value)(nextPos.value)
        e = nextPos
      }
      toReturn
    }else{
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position > toPosIncluded) {
        val prevPos = e.prev.get
        toReturn += distanceMatrixOnNode(e.value)(prevPos.value) + distanceMatrixOnNode(prevPos.value)(prevPos.value)
        e = prevPos
      }
      toReturn
    }
  }

  /**
   *
   * @param s a sequence of integers representing routes
   * @return the distance per vehicle or the total distance in a singleton array, according to the global "perVehicle" flag
   */
  private def computeValueFromScratch(s:IntSequence):Array[Int] = {
    val toReturn = Array.tabulate(v)(v => distanceMatrixOnNode(v)(v))
    val it = s.iterator

    var prevNode:Int = it.next()
    var currentVehicle:Int = prevNode
    require(currentVehicle == 0)

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle (cost of vehicle node already added)
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(node) +  distanceMatrixOnNode(node)(node)
      }
      prevNode = node
    }
    //for the last vehicle, the finishing operation in the loop will not be executed, so we have to add one here
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)

    if(perVehicle) toReturn
    else Array.fill(1)(toReturn.sum)
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c, routes.value)
  }

  def check(c : Checker,s:IntSequence) {
    c.check(!distanceIsSymmetric || ConstantRoutingDistance.isDistanceSymmetric(distanceMatrixOnNode, n), Some("distance matrix should be symmetric if invariant told so"))

    if (perVehicle) {
      val values = computeValueFromScratch(s)
      for (vehicle <- 0 to v - 1) {
        c.check(distance(vehicle).newValue == values(vehicle), Some("distance(" + vehicle + ").value=" + distance(vehicle).newValue + " should == computeValueFromScratch(routes.value)(0)" + values(vehicle)))
      }

      if (checkpoint != null) {
        val values = computeValueFromScratch(checkpoint)
        for (vehicle <- 0 to v - 1) {
          if (isVehicleChangedSinceCheckpoint(vehicle))
            c.check(savedValues(vehicle) == values(vehicle))
        }
      }

    } else {
      c.check(distance(0).newValue == computeValueFromScratch(s)(0), Some("distance(0).value=" + distance(0).newValue + " should== computeValueFromScratch(routes.value)(0)" + computeValueFromScratch(routes.value)(0)))
      if (checkpoint != null) {
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
                                        n:Int,
                                        v:Int,
                                        distanceMatrix:Array[Array[Int]],
                                        distance:Array[CBLSIntVar],
                                        distanceIsSymmetric:Boolean,
                                        precomputeFW:Boolean,
                                        precomputeBW:Boolean)
  extends ConstantRoutingDistance(routes:ChangingSeqValue,
    n,
    v:Int,
    distanceMatrix:Array[Array[Int]],
    distance:Array[CBLSIntVar],
    distanceIsSymmetric:Boolean) {

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
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).get.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => false //we finished the last vehicle
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1){
          precomputedForwardCumulatedCostAtCheckpoint(value) =
            precomputedForwardCumulatedCostAtCheckpoint(prevValue) +
              distanceMatrixOnNode(prevValue)(value) +
              distanceMatrixOnNode(prevValue)(prevValue)
          explorerOPt = explorer.next
          prevValue = value
          true
        } else false
    }){}
  }

  def computePrecomputedBackwardCumulatedCostAtCheckpoint(vehicle : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).get.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => false
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1) {
          precomputedBackwardCumulatedCostAtCheckpont(value) =
            precomputedBackwardCumulatedCostAtCheckpont(prevValue) +
              distanceMatrixOnNode(value)(prevValue) +
              distanceMatrixOnNode(value)(value)
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
      distanceMatrixOnNode(fromValueIncluded)(fromValueIncluded)
    } else if(((atCheckpoint && !perVehicle)|| (perVehicle && !isVehicleChangedSinceCheckpoint(vehicle)))&& precomputeFW && forwardRequired){
      //we need the or here above because we could be in single vehicle mode, where isVehicleChanged is always true
      //Forward
      doFWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded)
        + distanceMatrixOnNode(fromValueIncluded)(fromValueIncluded)
        - precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
      toReturn
    }else if(atCheckpoint && precomputeBW && !forwardRequired) {
      //backward
      doBWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(fromValueIncluded)
        + distanceMatrixOnNode(toValueIncluded)(toValueIncluded)
        - precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded, toPosIncluded, toValueIncluded))
      toReturn
    }else if(((atCheckpoint && !perVehicle) || (perVehicle && !isVehicleChangedSinceCheckpoint(vehicle))) && (precomputeFW || precomputeBW) && distanceIsSymmetric){
      //gt the other distance from the available pre-compute
      if(precomputeFW){
        //getting a BW distance from a FW precompute
        doFWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded)
          + distanceMatrixOnNode(toValueIncluded)(toValueIncluded)
          - precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded))
        assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
        toReturn
      }else{
        //getting a FW distance from a BW precompute
        doBWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded)
          + distanceMatrixOnNode(fromValueIncluded)(fromValueIncluded)
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

    var routesToPrecompute = changedVehiclesSinceCheckpoint
    while(routesToPrecompute != null){
      val currentRoute=routesToPrecompute.head
      routesToPrecompute = routesToPrecompute.tail
      isFWPrecomputeUpToDate(currentRoute) = false
      isBWPrecomputeUpToDate(currentRoute) = false
    }

    super.saveCurrentCheckpoint(s)
  }
}
