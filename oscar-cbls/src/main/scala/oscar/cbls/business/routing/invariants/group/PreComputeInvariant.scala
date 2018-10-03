package oscar.cbls.business.routing.invariants.group

import oscar.cbls.Variable
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
abstract class PreComputeInvariant[T: Manifest, U:Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n = routes.maxValue+1
  val vehicles = 0 until v

  val preComputedValues: Array[T] = new Array[T](n)
  val vehicleValues : Array[U] = new Array[U](v)
  var checkpointAtLevel0: IntSequence = _
  var checkpoint0Defined = false
  var changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, false)

  val checkpointStack = new SeqCheckpointedValueStack[(FunctionForPreCompute, VehicleLocation, Array[U])]

  private var bijForPreCompute: FunctionForPreCompute = _
  private var stackDone = false // is the current bijection a stacked bijection or not
  private var prevRoutes = routes.newValue /* the route before teh last change. We use it
  in fromScratchToNode if the bijection is a stacked bijection*/

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  //computeAndAffectOutputFromScratch(routes.value) // initialize the output of the invariant

  registerStaticAndDynamicDependency(routes)


  finishInitialization()

  for (outputVariable <- outputVariables){outputVariable.setDefiningInvariant(this)}

  for (vehicle <- vehicles){
    val valueOfVehicle = computeVehicleValueFromScratch(vehicle,routes.value)
    assignVehicleValue(vehicle,valueOfVehicle)
    vehicleValues(vehicle) = valueOfVehicle
  }


  def outputVariables:Iterable[Variable]


  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    * @param vehicle the vehicle where pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param setNodeValue the method that you are expected to use when assigning a value to a node
    *                     BEWARE: you can only apply this method on nodes of the vehicle you are working on
    * @param getNodeValue a method that you can use to get the value associated wit ha node
    *                     BEWARE: you have zero info on when it can generated, so only query the value
    *                     that you have just set through the method setNodeValue.
    *                     also, you should only query the value of node in the route of vehicle "vehicle"
    */
  def performPreCompute(vehicle:Int,
                        routes:IntSequence,
                        preComputedVals:Array[T])

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes the sequence representing the route of all vehicle
    * @param nodeValue a function that you can use to get the pre-computed value associated with each node (if some has ben given)
    *                  BEWARE: normally, you should never use this function, you only need to iterate through segments
    *                  because it already contains the pre-computed values at the extremity of each segment
    * @return the value associated with the vehicle
    */
  def computeVehicleValue(vehicle:Int,
                          segments:List[Segment[T]],
                          routes:IntSequence,
                          preComputedVals:Array[T]):U

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    * @param vehicle the vehicle number
    * @param value the value of the vehicle
    */
  def assignVehicleValue(vehicle:Int,value:U)

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */

  def computeVehicleValueFromScratch(vehicle : Int, routes : IntSequence):U

  def valuesToSave() : Array[U] = {
    Array.tabulate(v)(vehicleValues)
  }

  def restoreValueAtCheckpoint(valueAtCheckpoint : Array[U],checkPointLevel:Int): Unit ={
    for (vcl <- vehicles){
      if (vehicleValues(vcl) != valueAtCheckpoint(vcl)) {
        vehicleValues(vcl) = valueAtCheckpoint(vcl)
        assignVehicleValue(vcl,valueAtCheckpoint(vcl))
      }
    }
  }

  /**
    *
    * @param computationStep
    * @return
    */
  def convertComputationStepToSegment(computationSteps : List[ComputationStep],routes : IntSequence,prevRoutes : IntSequence): List[Segment[T]] ={

    //println(computationSteps.mkString(","))
    val toReturn =
      computationSteps.flatMap(step => {
        step match {
          case FetchFromPreCompute(startNodePosition, endNodePosition, rev) =>
            val realStartNodePosition = bijection(startNodePosition)
            val realEndNodePosition = bijection(endNodePosition)
            val startNode = prevRoutes.valueAtPosition(realStartNodePosition).get
            val endNode = prevRoutes.valueAtPosition(bijection (realEndNodePosition)).get
            if (!rev) {
              Some (PreComputedSubSequence(startNode, preComputedValues(startNode), endNode, preComputedValues(endNode)))
            } else {
              Some (FlippedPreComputedSubSequence(endNode,preComputedValues(endNode),startNode,preComputedValues(startNode)))
            }
          case FromScratch(fromNode,toNode,topOfStack) =>
            var newNodeList:List[NewNode[T]] = Nil
            for (nodePos <- fromNode to toNode) {
              val node = (
                if (topOfStack)
                  routes.valueAtPosition(nodePos).get
                else
                  prevRoutes.valueAtPosition(nodePos).get)
              newNodeList = NewNode[T](node)::newNodeList
            }
            newNodeList.reverse
        }
      })
    //println(toReturn.mkString(","))
    toReturn
  }

  /**
    *
    * @param fs a segment which need from scratch computation
    * @return the nodes at the extremities of segment
    */
  def fromScratchToNode (fs: FromScratch): (Int, Int) = {
    if (!stackDone){
      // no stack donen so we can use the current route to know the nodes
      val fromNode = routes.newValue.valueAtPosition(fs.fromPosAtCheckpoint0).get
      val toNode = routes.newValue.valueAtPosition(fs.toPosAtCheckpoint0).get
      (fromNode, toNode)
    }
    else{
      // current bijection is stacked. If fs is not the last inserted node, we need to look at the previous value of the routes
      if (fs.topOfStack){
        require(fs.fromPosAtCheckpoint0 == fs.toPosAtCheckpoint0)
        val node = routes.newValue.valueAtPosition(fs.fromPosAtCheckpoint0).get
        (node, node)
      }
      else{
        val fromNode = prevRoutes.valueAtPosition(fs.fromPosAtCheckpoint0).get
        val toNode = prevRoutes.valueAtPosition(fs.toPosAtCheckpoint0).get
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
      case None => for (vehicle <- vehicles){
        assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
      }
      case Some(x) if !x.checkpoint0Defined =>
        for (vehicle <- vehicles){
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
          case Some(u) if u.checkpoint0Defined && checkpointLevel != 0 => u.updatedBij.commitStackedUpdatesToConcrete()
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
          for (vehicle <- 0 until v) performPreCompute(vehicle,routes.value,preComputedValues)
        }
        else{
          if(checkpointLevel == 0){
            for (vehicle <- prevUpdates.get.changedVehicleSinceCheckpoint0.indicesAtTrue)
              performPreCompute(vehicle,routes.value,preComputedValues)
          }
        }

        val updates = prevUpdates match {
          case None =>
            new UpdatedValues(IterableMagicBoolArray(v, true), bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)
          case Some(u) =>
            /*
            * Thanks to the initialization and the from scratch procedure, we can compute the output only on changed vehicle.
            * Even if we are creating the first checkpoint
            * */
            new UpdatedValues(u.changedVehicle, bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)

        }
        applyUpdates(updates,changes.newValue) //we need to compute the output when we are defining a checkpoint for restore

        val toSave = valuesToSave()
        checkpointStack.defineCheckpoint(s.newValue, checkpointLevel, (bijAtCheckpoint, vehicleSearcherAtCheckpoint, toSave))
        val toReturn = new UpdatedValues(IterableMagicBoolArray(v), bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, newCheckpoint0, newCHangedVehicleSinceCheckpoint0)
        Some(toReturn)

      case s@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel) =>
        val (bijAtCheckpoint, vehicleSearcherAtCheckpoint, valueAtCheckpoint) = checkpointStack.rollBackAndOutputValue(checkpoint, checkpointLevel)
        restoreValueAtCheckpoint(valueAtCheckpoint, checkpointLevel)
        val changedVehicle = IterableMagicBoolArray(v)
        val toReturn = new UpdatedValues(changedVehicle, bijAtCheckpoint, vehicleSearcherAtCheckpoint, true, false, s.newValue, checkpointAtLevel0, changedVehiclesSinceCheckpoint0)
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
          val toReturn = new UpdatedValues(updatedChangedVehicle, stackedBij, updateVehicleSearcher, true, true, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)

          case Some(updates) =>
            // checkpoint is not defined. So the bijection doesn't exist. we can't update it
            val updateVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val vehicle  = updateVehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val toReturn = new UpdatedValues(updatedChangedVehicle, updates.updatedBij, updateVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
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
            val toReturn = new UpdatedValues(updatedChangedVehicle, stackedBij, updatedVehicleSearcher, true, true, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)

          case Some(updates) =>
            // same as insert
            val vehicle = updates.vehicleSearcher.vehicleReachingPosition(pos)
            val updatedChangedVehicle = updates.changedVehicle
            updatedChangedVehicle(vehicle) = true
            val updatedChangedVehicleSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
            updatedChangedVehicleSinceCheckpoint0(vehicle) = true
            val updatedVehicleSearcher = updates.vehicleSearcher.push(s.oldPosToNewPos)
            val toReturn = new UpdatedValues(updatedChangedVehicle, updates.updatedBij, updatedVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
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

            Some(new UpdatedValues(
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
            val toReturn = new UpdatedValues(updatedChangedVehicle, updates.updatedBij, updatedVehicleSearcher, false, false, prev.newValue, updates.checkpoint0, updatedChangedVehicleSinceCheckpoint0)
            Some(toReturn)
        }

      case SeqUpdateLastNotified(value: IntSequence) =>
        require(value quickEquals routes.value)
        val initValue = new UpdatedValues(IterableMagicBoolArray(v), bijForPreCompute, vehicleSearcher, checkpoint0Defined, stackDone, prevRoutes, checkpointAtLevel0, changedVehiclesSinceCheckpoint0)
        // we start with current values associated at the route
        Some(initValue)

      case SeqUpdateAssign(value: IntSequence) =>
        None
    }
  }

  /**
    * affect to global variables the values contained in updates. Then for all vehicles impacted by the last changes, compute
    * and affect the new value of the outputs’effondrer. C’est ce qui s’est passé en Australie, comme l’explique Joe
    * @param updates obtained from digestUpdates
    */
  private def applyUpdates(updates: UpdatedValues,routes : IntSequence): Unit = {
    bijForPreCompute = updates.updatedBij
    checkpoint0Defined = updates.checkpoint0Defined
    stackDone = updates.stackDone
    vehicleSearcher = updates.vehicleSearcher
    prevRoutes = updates.prevRoutes
    checkpointAtLevel0 = updates.checkpoint0
    changedVehiclesSinceCheckpoint0 = updates.changedVehicleSinceCheckpoint0
    for (vehicle <- updates.changedVehicle.indicesAtTrue){
      val posOfLastNode =
        if(vehicle != this.v-1) routes.explorerAtAnyOccurrence(vehicle+1).get.position - 1
        else bijForPreCompute.externalPositionOfLastRoutedNode
      val posOfFirstNode = routes.explorerAtAnyOccurrence(vehicle).get.position
      val value = computeVehicleValue(vehicle,convertComputationStepToSegment(bijForPreCompute.kindOfComputation(posOfFirstNode,posOfLastNode),routes,prevRoutes),routes,preComputedValues)
      assignVehicleValue(vehicle,value)
      vehicleValues(vehicle) = value
    }
  }

  def preComputedToString():String = {
    var res = "["
    for (i <- 0 until preComputedValues.length - 2) {
      if (preComputedValues(i) != 0){
        res = res + "(" + i + " : " + preComputedValues(i).toString() + "), "
      }
    }
    if (preComputedValues(preComputedValues.length -1) != 0){
      res = res + "(" + (preComputedValues.length - 1).toString() + " : " + preComputedValues(preComputedValues.length - 1).toString() + ")"
    }
    res = res + "]"
    res
  }

  override def checkInternals(c : Checker): Unit = {
    for (v <- vehicles){
      require(computeVehicleValueFromScratch(v,routes.value) == vehicleValues(v),
        "For Vehicle " + v.toString() + " : " + computeVehicleValueFromScratch(v,routes.value).toString() + " " + vehicleValues(v).toString() + " " + routes.toString + "\n" + preComputedToString())
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
class UpdatedValues(val changedVehicle: IterableMagicBoolArray,
                    val updatedBij: FunctionForPreCompute,
                    val vehicleSearcher: VehicleLocation,
                    val checkpoint0Defined: Boolean,
                    val stackDone: Boolean,
                    val prevRoutes: IntSequence,
                    val checkpoint0: IntSequence,
                    val changedVehicleSinceCheckpoint0: IterableMagicBoolArray)
