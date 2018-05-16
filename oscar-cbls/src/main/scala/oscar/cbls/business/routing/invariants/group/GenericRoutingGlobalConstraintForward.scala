package oscar.cbls.business.routing.invariants.group

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

import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core._

/**
  * @author Quentin Meurisse
  *
  * @param routes
  * @param v number of vehicle
  * @tparam T type of pre-computes used by the invariant
  * @tparam U type of the output of the invariant
  */
@deprecated("not enough validation yet, use this at your own risk","")
abstract class GenericRoutingGlobalConstraintForward[T: Manifest, U](routes: ChangingSeqValue, n:Int, v: Int)
  extends Invariant with SeqNotificationTarget{

  val vehicles = 0 until v

  val preComputes: Array[T] = new Array[T](n)
  var checkpointAtLevel0: IntSequence = _
  var checkpoint0Defined = false
  var changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, false)

  val checkpointStack = new SeqCheckpointedValueStack[(FunctionForPreCompute, VehicleLocation, U)]

  private var bijForPreCompute: FunctionForPreCompute = null

  //is the current bijection a stacked bijection or not
  private var stackDone = false

  //the route before teh last change. We use it
  //in fromScratchToNode if the bijection is a stacked bijection
  private var prevRoutes = routes.newValue

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  computeAndAffectOutputFromScratch(routes.value) // initialize the output of the invariant

  /**
    * we consider that start point of all vehicles have the neutral element associated to them.
    * @return an element t of type T such as for all x of type T, x + t = x and x-t = x
    */
  def neutralElement: T

  /**
    *
    * @param x
    * @param y
    * @param reverse is the segment flipped or not
    * @return y - x
    */
  def minus(x: T, y: T, reverse:Boolean = false): T

  /**
    *
    * @param x
    * @param y
    * @return x + y
    */
  def plus(x: T, y: T): T

  /**
    * this method is expected to return Sum(fromNode ... toNode) from scratch
    * @param fromNode
    * @param toNode
    * @return the difference of pre-computes on segment fromNode -> ... -> toNode but here the difference is computed from scratch
    */
  def computeDeltaBetweenNodesFromScatch(fromNode: Int, toNode:Int): T

  def computeAndAffectOutputFromScratch(seq: IntSequence)

  /**
    * Compute the output with value and affect it
    * @param value the result of computeNewValueOfPreComputeForNodeAtPos
    *              where the node is the last node of the route of vehicle
    * @param vehicle
    */
  def computeAndAffectOutputWithPreCompute(value: T, vehicle: Int)


  def restoreValueAtCheckpoint(value: U, checkpointLevel: Int)

  /**
    * Do pre-compute for a vehicle. We do the pre-compute only at checkpoint of level 0.
    * @param vehicle
    */
  def doPreComputeAtCheckpoint0(vehicle: Int,checkpoint:IntSequence)

  /**
    * @return the actual output of the invariant to save it at the current checkpoint
    */
  def valuesToSave(): U

  /**
    *
    * @param pos
    * @param vehicle
    * @return thanks the bijection and the operators plus and minus, we can compute the value of a pre-compute for a node.
    *         To do that, we do a cumulative sum of the difference of the pre-compute on the segment which compose the bijection
    */
  def computeNewValueOfPreComputeForNodeAtPos(pos: Int, vehicle: Int): T = {
    val vehiclePos = routes.newValue.positionOfAnyOccurrence(vehicle).get
    val computationSteps = bijForPreCompute.kindOfComputation(vehiclePos, pos)
    var newValue: T = neutralElement // we need to define a neutral element for initialize the cumulative sum
    for (step <- computationSteps.toList){
      step match {
        case FetchFromPreCompute(from, to, false) =>
          val x =
            if (checkpointAtLevel0.valueAtPosition(bijection(from)).get < v) neutralElement //by convention, the startpoint of a vehicle has the neutral element associated to it.
            else preComputes(checkpointAtLevel0.valueAtPosition(bijection(from) - 1).get)
          val y = preComputes(checkpointAtLevel0.valueAtPosition(bijection(to)).get)
          newValue = plus(newValue, minus(x, y))
        case FetchFromPreCompute(from, to, true) =>
          val x = preComputes(checkpointAtLevel0.valueAtPosition(bijection(from)).get)
          val y = preComputes(checkpointAtLevel0.valueAtPosition(bijection(to) - 1).get) // a vehicle can't change its start point. So for a flipped segment fromValue >= v
          newValue = plus(newValue, minus(y, x, reverse = true))
        case fs@FromScratch(fromPos, toPos, topOfStack) =>
          val (fromNode, toNode) = fromScratchToNode(fs)
          newValue = plus(newValue, computeDeltaBetweenNodesFromScatch(fromNode, toNode))
      }
    }
    newValue
  }

  /**
    * @param fs a segment which need from scratch computation
    * @return the nodes at the extremities of segment
    */
  def fromScratchToNode (fs: FromScratch): (Int, Int) = {
    if (!stackDone){
      // no stack donen so we can use the current route to know the nodes
      val fromNode = routes.newValue.valueAtPosition(fs.fromPos).get
      val toNode = routes.newValue.valueAtPosition(fs.toPos).get
      (fromNode, toNode)
    } else{
      // current bijection is stacked. If fs is not the last inserted node, we need to look at the previous value of the routes
      if (fs.topOfStack){
        require(fs.fromPos == fs.toPos)
        val node = routes.newValue.valueAtPosition(fs.fromPos).get
        (node, node)
      }
      else{
        val fromNode = prevRoutes.valueAtPosition(fs.fromPos).get
        val toNode = prevRoutes.valueAtPosition(fs.toPos).get
        (fromNode, toNode)
      }
    }
  }

  def recordTouchedVehicleSinceCheckpoint0(vehicle:Int){
    changedVehiclesSinceCheckpoint0(vehicle) = true
  }

  def bijection(x: Int): Int = {
    bijForPreCompute.fun(x)
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) = {
    val updates = digestUpdates(changes)
    updates match{
      case None => computeAndAffectOutputFromScratch(changes.newValue)
      case Some(x) if !x.checkpoint0Defined => computeAndAffectOutputFromScratch(changes.newValue)
      case Some(x) => applyUpdates(x)
    }
  }

  private def digestUpdates(changes: SeqUpdate): Option[UpdatedValues] = {
    changes match {
      case s@SeqUpdateDefineCheckpoint(prev: SeqUpdate, activeCheckpoint: Boolean, checkpointLevel) =>
        val prevUpdates = digestUpdates(prev)

        val checkpoint0WasDefined = prevUpdates match {
          case None => false
          case Some(x) => x.checkpoint0Defined
        }

        val bijAtCheckpoint = prevUpdates match {
          case None => ConcreteFunctionForPreCompute(s.newValue)
          case Some(u) if u.checkpoint0Defined && checkpointLevel != 0 => u.updatedBij.concreteFunction
          case Some(u) => ConcreteFunctionForPreCompute(s.newValue)
        }

        val vehicleSearcherAtCheckpoint = prevUpdates match {
          case None => this.vehicleSearcher.regularize
          case Some(x) => x.vehicleSearcher.regularize
        }

        val newCheckpoint0 =
          if (checkpointLevel == 0) s.newValue
          else prevUpdates.get.checkpoint0

        val newCHangedVehicleSinceCheckpoint0 =
          if(checkpointLevel == 0) IterableMagicBoolArray(v)
          else prevUpdates.get.changedVehicleSinceCheckpoint0

        if (!checkpoint0WasDefined){
          // we are creating the first checkpoint. We need to do pre-compute for all vehicle
          for (vehicle <- 0 until v) doPreComputeAtCheckpoint0(vehicle,newCheckpoint0)
        }
        else{
          if(checkpointLevel == 0){
            for (vehicle <- prevUpdates.get.changedVehicleSinceCheckpoint0.indicesAtTrue)
              doPreComputeAtCheckpoint0(vehicle,newCheckpoint0)
          }
        }

        val updates = prevUpdates match {
          case None =>
            UpdatedValues(IterableMagicBoolArray(v, true), bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)
          case Some(u) =>
            /*
            * Thanks to the initialization and the from scratch procedure, we can compute the output only on changed vehicle.
            * Even if we are creating the first checkpoint
            * */
            UpdatedValues(u.changedVehicle, bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)

        }
        applyUpdates(updates) //we need to compute the output when we are defining a checkpoint for restore

        val toSave = valuesToSave()
        checkpointStack.defineCheckpoint(s.newValue, checkpointLevel, (bijAtCheckpoint, vehicleSearcherAtCheckpoint, toSave))
        val toReturn = UpdatedValues(IterableMagicBoolArray(v), bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)
        Some(toReturn)

      case s@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel) =>
        val (bijAtCheckpoint, vehicleSearcherAtCheckpoint, valueAtCheckpoint) = checkpointStack.rollBackAndOutputValue(checkpoint, checkpointLevel)
        restoreValueAtCheckpoint(valueAtCheckpoint, checkpointLevel)
        val changedVehicle = IterableMagicBoolArray(v)
        val toReturn = UpdatedValues(changedVehicle, bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, checkpointAtLevel0, changedVehiclesSinceCheckpoint0)
        Some(toReturn)

      case s@SeqUpdateInsert(value: Int, pos: Int, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)
        prevUpdates match {
          case None => None

          case Some(updates) if updates.checkpoint0Defined =>
            // if a level 0 checkpoint was defined, the bijection exists. So we can update it
            val updateVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val vehicle  = updateVehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val stackedBij = updates.updatedBij.stackInsert(value, pos) // we stack an insert
          val toReturn = UpdatedValues(updatedChangedVehicle, stackedBij, updateVehicleSearcher, true, true, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)

          case Some(updates) =>
            // checkpoint is not defined. So the bijection doesn't exist. we can't update it
            val updateVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val vehicle  = updateVehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val toReturn = UpdatedValues(updatedChangedVehicle, updates.updatedBij, updateVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)
        }

      case s@SeqUpdateRemove(pos: Int, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)

        prevUpdates match {
          case None => None

          case Some(updates) if updates.checkpoint0Defined =>
            // same as insert
            val vehicle = updates.vehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val updatedVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val stackedBij = updates.updatedBij.stackDelete(pos)
            val toReturn = UpdatedValues(updatedChangedVehicle, stackedBij, updatedVehicleSearcher, true, true, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)

          case Some(updates) =>
            // same as insert
            val vehicle = updates.vehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val updatedVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val toReturn = UpdatedValues(updatedChangedVehicle, updates.updatedBij, updatedVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)
        }

      case s@SeqUpdateMove(fromPosIncluded: Int, toPosIncluded: Int, afterPos: Int, flip: Boolean, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)

        prevUpdates match{
          case None => None

          case Some(updates) if updates.checkpoint0Defined =>
            // same as insert
            val sourceVehicleOfMove = updates.vehicleSearcher.vehicleReachingPosition(fromPosIncluded)
            val targetVehicleOfMove = updates.vehicleSearcher.vehicleReachingPosition(afterPos)
            val updatedVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(sourceVehicleOfMove) = true
            updatedChangedVehicle(targetVehicleOfMove) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(sourceVehicleOfMove) = true
            updatedChangedVehicleSinceCheckpoint0(targetVehicleOfMove) = true
            val stackedBij = updates.updatedBij.stackMove(fromPosIncluded, toPosIncluded, afterPos, flip)

            Some(UpdatedValues(
              updatedChangedVehicle,
              stackedBij,
              updatedVehicleSearcher,
              true,
              true,
              prev.newValue,
              updates.checkpoint0,
              updatedChangedVehicleSinceCheckpoint0))


          case Some(updates) =>
            // same as insert
            val sourceVehicleOfMove = updates.vehicleSearcher.vehicleReachingPosition(fromPosIncluded)
            val targetVehicleOfMove = updates.vehicleSearcher.vehicleReachingPosition(afterPos)
            val updatedVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(sourceVehicleOfMove) = true
            updatedChangedVehicle(targetVehicleOfMove) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(sourceVehicleOfMove) = true
            updatedChangedVehicleSinceCheckpoint0(targetVehicleOfMove) = true
            val toReturn = UpdatedValues(updatedChangedVehicle, updates.updatedBij, updatedVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)
        }

      case SeqUpdateLastNotified(value: IntSequence) =>
        require(value quickEquals routes.value)
        val initValue = UpdatedValues(IterableMagicBoolArray(v), bijForPreCompute, vehicleSearcher, checkpoint0Defined, stackDone, prevRoutes, checkpointAtLevel0, changedVehiclesSinceCheckpoint0)
        // we start with current values associated at the route
        Some(initValue)

      case SeqUpdateAssign(value: IntSequence) =>
        None
    }
  }

  /**
    * affect to global variables the values contained in updates. Then for all vehicles impacted by the last changes, compute
    * and affect the new value of the output
    * @param updates obtained from digestUpdates
    */
  private def applyUpdates(updates: UpdatedValues): Unit = {
    bijForPreCompute = updates.updatedBij
    checkpoint0Defined = updates.checkpoint0Defined
    stackDone = updates.stackDone
    vehicleSearcher = updates.vehicleSearcher
    prevRoutes = updates.prevRoutes
    checkpointAtLevel0 = updates.checkpoint0
    changedVehiclesSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
    for (vehicle <- updates.changedVehicle.indicesAtTrue){
      val posOfLastNode =
        if(vehicle != this.v-1) routes.newValue.explorerAtAnyOccurrence(vehicle+1).get.position - 1
        else bijForPreCompute.externalPositionOfLastRoutedNode
      val value = computeNewValueOfPreComputeForNodeAtPos(posOfLastNode, vehicle)
      computeAndAffectOutputWithPreCompute(value, vehicle)
    }
  }
}

/**
  * class which contains values updated by digestUpdates
  * @param changedVehicle vehicles impacted by the last changes
  * @param updatedBij bijection modify by last changes
  * @param vehicleSearcher vehicle searcher modified by last changes
  * @param checkpoint0Defined if a checkpoint of level 0 was defined
  * @param stackDone if the current bijection is a stacked bijection
  * @param prevRoutes routes before the last change
  */
case class UpdatedValues(changedVehicle: IterableMagicBoolArray,
                         updatedBij: FunctionForPreCompute,
                         vehicleSearcher: VehicleLocation,
                         checkpoint0Defined: Boolean,
                         stackDone: Boolean,
                         prevRoutes: IntSequence,
                         checkpoint0: IntSequence,
                         changedVehicleSinceCheckpoint0: IterableMagicBoolArray)
