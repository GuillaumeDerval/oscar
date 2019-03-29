package oscar.cbls.business.routing.invariants.group

import oscar.cbls.Variable
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core._
import oscar.cbls._

/**
  * @author Quentin Meurisse
  *
  * @param routes
  * @param v number of vehicle
  * @tparam T type of pre-computes used by the invariant
  * @tparam U type of the output of the invariant
  */
abstract class GlobalConstraintDefinition[T : Manifest, U:Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n = routes.maxValue+1L
  val vehicles = 0L until v

  val preComputedValues: Array[T] = new Array[T](n)
  val vehicleValues : Array[U] = new Array[U](v)
  var checkpointAtLevel0: IntSequence = _
  var checkpoint0Defined = false
  var changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, false)
  var changedVehiclesSinceLastApplyUpdate = new IterableMagicBoolArray(v,false)

  val checkpointStack = new SeqCheckpointedValueStack[(FunctionForPreCompute,VehicleLocation,List[ModifiedValues[U]])]

  private var bijForPreCompute: FunctionForPreCompute = _
  private var stackDone = false // is the current bijection a stacked bijection or not
  private var prevRoutes = routes.newValue /* the route before teh last change. We use it
  in fromScratchToNode if the bijection is a stacked bijection*/
  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  var currentCheckpointLevel : Long = -1L
  var currentCheckpointRoute : IntSequence = routes.newValue
  var currentCheckpointBijection = bijForPreCompute
  var currentCheckpointSearcher = vehicleSearcher
  var currentCheckpointHowToRollBack : List[ModifiedValues[U]] = Nil



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
    * @param preComputedVals The array of precomputed values
    */
  def performPreCompute(vehicle:Long,
                        routes:IntSequence,
                        preComputedVals:Array[T])

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes the sequence representing the route of all vehicle
    * @param preComputedVals The array of precomputed values
    * @return the value associated with the vehicle
    */
  def computeVehicleValue(vehicle:Long,
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
  def assignVehicleValue(vehicle:Long,value:U)

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */

  def computeVehicleValueFromScratch(vehicle : Long, routes : IntSequence):U


  def restoreListValues(valueAtCheckpoint : List[ModifiedValues[U]]) : Unit = {
    valueAtCheckpoint match{
      case Nil =>
      case head :: queue =>
        vehicleValues(head.vehicle) =  head.value
        assignVehicleValue(head.vehicle,head.value)
        changedVehiclesSinceCheckpoint0(head.vehicle) = head.hasChangedSinceCheckpoint0
        restoreListValues(queue)
    }
  }

  def restoreValueAtCheckpoint(checkpoint : IntSequence,checkpointLevel:Long): Unit ={
 //   println("Roll Back - Actual Chkpnt Lvl : " + currentCheckpointLevel + " Chkpnt to restore : " + checkpointLevel)
    if (checkpointLevel != currentCheckpointLevel){
      restoreListValues(currentCheckpointHowToRollBack)
      val (bij,search,rb) = checkpointStack.rollBackAndOutputValue(checkpoint,checkpointLevel)
      currentCheckpointBijection = bij
      currentCheckpointSearcher = search
      currentCheckpointHowToRollBack = rb
    }
    bijForPreCompute = currentCheckpointBijection
    vehicleSearcher = currentCheckpointSearcher
    restoreListValues(currentCheckpointHowToRollBack)
    currentCheckpointLevel = checkpointLevel
    currentCheckpointRoute = checkpoint
    currentCheckpointHowToRollBack = Nil
 //   println("New Checkpoint Level : " + checkpointLevel)
 //   println("Route After Checkpoint : ")
 //   println(currentCheckpointRoute)
  }

  /** This method convert the computation steps received from the sequence into the segemnt type
    *
    * @param computationStep the list of computation steps
    * @param routes the actual route
    * @param prevRoutes the previous route before the steps
    *
    * @return the list of segment corresponding to the computation steps
    */
  def convertComputationStepToSegment(computationSteps : List[ComputationStep],routes : IntSequence,prevRoutes : IntSequence): List[Segment[T]] ={

    //println(computationSteps.mkString(","))
    //println("Actual route : " + routes)
    //println("Previous route : " + prevRoutes)
    val toReturn =
      computationSteps.flatMap(step => {
        step match {
          case FetchFromPreCompute(startNodePosition, endNodePosition, rev) =>
  //          val realStartNodePosition = bijection(startNodePosition)
  //          val realEndNodePosition = bijection(endNodePosition)
  //          println("Start Node position : " + startNodePosition + " - Node at startNodePosition : " + prevRoutes.valueAtPosition(startNodePosition).get + " -- RealStartNodePosition : " + realStartNodePosition + " - Node atRealStartNodePOsitiono : " + prevRoutes.valueAtPosition(realStartNodePosition).get)
  //          println("End Node position : " + endNodePosition + " - Node at endNodePosition : " + prevRoutes.valueAtPosition(endNodePosition).get + " -- RealEndNodePosition : " + realEndNodePosition + " - Node atRealEndNodePOsitiono : " + prevRoutes.valueAtPosition(realEndNodePosition).get)
            //TODO: improve this; valueAtPosition is where most of the time is wasted.
            val startNode = prevRoutes.valueAtPosition(startNodePosition).get
            val endNode = prevRoutes.valueAtPosition(endNodePosition).get
            //println("NextEndNode : " + explorer.get.value)
            if (!rev) {
              Some (PreComputedSubSequence(startNode, preComputedValues(startNode), endNode, preComputedValues(endNode)))
            } else {
              Some (FlippedPreComputedSubSequence(startNode,preComputedValues(startNode),endNode,preComputedValues(endNode)))
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
    toReturn
  }

  /**
    *
    * @param fs a segment which need from scratch computation
    * @return the nodes at the extremities of segment
    */
  def fromScratchToNode (fs: FromScratch): (Long, Long) = {
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


  def bijection(x: Long): Long = {
    bijForPreCompute.fun(x)
  }


  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val updates = digestUpdates(changes)
    updates match{
      case None => for (vehicle <- vehicles){
        assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
      }
      case Some(x) if !checkpoint0Defined =>
        for (vehicle <- vehicles){
          assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
        }
      case Some(x) => applyUpdates(x,changes.newValue)
    }
  }

  private def digestUpdates(changes: SeqUpdate): Option[UpdatedValues] = {
    changes match {
      case s@SeqUpdateDefineCheckpoint(prev: SeqUpdate, activeCheckpoint: Boolean, checkpointLevel) =>
        val prevUpdates = digestUpdates(prev)

        bijForPreCompute =
          if (checkpoint0Defined && checkpointLevel != 0L)
            bijForPreCompute.commitStackedUpdatesToConcrete()
          else
          ConcreteFunctionForPreCompute(s.newValue)

        vehicleSearcher = vehicleSearcher.regularize

        checkpointAtLevel0 = if (checkpointLevel == 0L) s.newValue else checkpointAtLevel0

        //println("Define Checkpoint : " + checkpointLevel)
        //println(changedVehiclesSinceCheckpoint0)
        if (!checkpoint0Defined){
          // we are creating the first checkpoint. We need to do pre-compute for all vehicle
          for (vehicle <- 0L until v) performPreCompute(vehicle,checkpointAtLevel0,preComputedValues)
        } else {
          if(checkpointLevel == 0L){
            //println("PreCompute :")
            //println(s.newValue)
            //println(changedVehiclesSinceCheckpoint0)
            for (vehicle <- changedVehiclesSinceCheckpoint0.indicesAtTrue)
              performPreCompute(vehicle,checkpointAtLevel0,preComputedValues)
          }
        }

        if (checkpoint0Defined) changedVehiclesSinceCheckpoint0.all_=(false)

        val updates = new UpdatedValues()

        if (prevUpdates.isEmpty) changedVehiclesSinceLastApplyUpdate.all_=(true)
        stackDone = false
        prevRoutes = s.newValue
        applyUpdates(updates,changes.newValue) //we need to compute the output when we are defining a checkpoint for restoration

        if (checkpointLevel != 0L){
          //println("Pushing ChckPnt")
          //println("At Checkpoint " + currentCheckpointLevel + " route is : " + currentCheckpointRoute)
          checkpointStack.defineCheckpoint(currentCheckpointRoute,currentCheckpointLevel,(currentCheckpointBijection,currentCheckpointSearcher,currentCheckpointHowToRollBack))
        }
        currentCheckpointLevel = checkpointLevel
        currentCheckpointRoute = s.newValue
        currentCheckpointSearcher = vehicleSearcher
        currentCheckpointBijection = bijForPreCompute
        currentCheckpointHowToRollBack = Nil
        //println("Define Checkpoint Level : " + checkpointLevel)
        //println(currentCheckpointRoute)
        checkpoint0Defined = true

        Some(new UpdatedValues())

      case s@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel) =>
        restoreValueAtCheckpoint(checkpoint, checkpointLevel)
        changedVehiclesSinceLastApplyUpdate.all_=(false)
        //println("Route After Checkpoint : ")
        //println(s.newValue)
        stackDone = false
        prevRoutes = s.newValue
        val toReturn = new UpdatedValues()
        Some(toReturn)

      case s@SeqUpdateInsert(value: Long, pos: Int, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)
        prevUpdates match {
          case None => None
          case Some(updates) =>
            vehicleSearcher = vehicleSearcher.push(s.oldPosToNewPos)
            val vehicle  = vehicleSearcher.vehicleReachingPosition(pos)
            changedVehiclesSinceLastApplyUpdate(vehicle) = true
            bijForPreCompute = if (checkpoint0Defined)  // if a level 0L checkpoint was defined, the bijection exists. So we can update it
              bijForPreCompute.stackInsert(value, pos) // we stack an insert
            else // else, the bijection does not exist
              bijForPreCompute
            stackDone = true
            prevRoutes = prev.newValue
            Some(new UpdatedValues())
        }

      case s@SeqUpdateRemove(pos: Int, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)
        prevUpdates match {
          case None => None
          case Some(updates) =>
            // same as insert
            val vehicle = vehicleSearcher.vehicleReachingPosition(pos)
            changedVehiclesSinceLastApplyUpdate(vehicle) = true
            vehicleSearcher = vehicleSearcher.push(s.oldPosToNewPos)
            bijForPreCompute =
              if (checkpoint0Defined)
                bijForPreCompute.stackDelete(pos)
              else
                bijForPreCompute
            stackDone = true
            prevRoutes = prev.newValue
            Some(new UpdatedValues())
        }

      case s@SeqUpdateMove(fromPosIncluded: Int, toPosIncluded: Int, afterPos: Int, flip: Boolean, prev: SeqUpdate) =>
        val prevUpdates = digestUpdates(prev)
        prevUpdates match{
          case None => None
          case Some(updates) =>
            val sourceVehicleOfMove = vehicleSearcher.vehicleReachingPosition(fromPosIncluded)
            val targetVehicleOfMove = vehicleSearcher.vehicleReachingPosition(afterPos)
            vehicleSearcher = vehicleSearcher.push(s.oldPosToNewPos)
            changedVehiclesSinceLastApplyUpdate(sourceVehicleOfMove) = true
            changedVehiclesSinceLastApplyUpdate(targetVehicleOfMove) = true
            bijForPreCompute =
              if (checkpoint0Defined)
                bijForPreCompute.stackMove(fromPosIncluded, toPosIncluded, afterPos, flip)
              else
                bijForPreCompute
            stackDone = true
            prevRoutes = prev.newValue
            Some(new UpdatedValues())
        }

      case SeqUpdateLastNotified(value: IntSequence) =>
        require(value quickEquals routes.value)
        changedVehiclesSinceLastApplyUpdate.all_=(false)
        val initValue = new UpdatedValues()
        Some(new UpdatedValues())

      case SeqUpdateAssign(value: IntSequence) =>
        checkpoint0Defined = false
        bijForPreCompute = ConcreteFunctionForPreCompute(value)
        None
    }
  }

  /**
    * affect to global variables the values contained in updates. Then for all vehicles impacted by the last changes, compute
    * and affect the new value of the output
    * @param updates obtained from digestUpdates
    */
  private def applyUpdates(updates: UpdatedValues,routes : IntSequence): Unit = {
    for (vehicle <- changedVehiclesSinceLastApplyUpdate.indicesAtTrue){
      val posOfLastNode =
        if(vehicle != this.v-1L) routes.explorerAtAnyOccurrence(vehicle+1L).get.position - 1L
        else bijForPreCompute.externalPositionOfLastRoutedNode
      val posOfFirstNode = vehicleSearcher.startPosOfVehicle(vehicle)
      val value = computeVehicleValue(vehicle,convertComputationStepToSegment(bijForPreCompute.kindOfComputation(posOfFirstNode,posOfLastNode),routes,prevRoutes),routes,preComputedValues)
      assignVehicleValue(vehicle,value)
      currentCheckpointHowToRollBack = new ModifiedValues(vehicle,vehicleValues(vehicle),changedVehiclesSinceCheckpoint0(vehicle))::currentCheckpointHowToRollBack
      changedVehiclesSinceCheckpoint0(vehicle) = true
      vehicleValues(vehicle) = value
      //println(vehicle + " : " + vehicleValues.mkString(","))
      //println(preComputedToString())
    }
    changedVehiclesSinceLastApplyUpdate.all_=(false);
  }

  def preComputedToString():String = {
    "[" + preComputedValues.indices.map(i => "\n\t" + i + ":" + preComputedValues(i)).mkString("") + "\n]"
  }

  override def checkInternals(c : Checker): Unit = {
    for (v <- vehicles){
      require(computeVehicleValueFromScratch(v,routes.value).equals(vehicleValues(v)),
        "For Vehicle " + v + " : " + computeVehicleValueFromScratch(v,routes.value) + " " + vehicleValues(v) + " " + routes + "\n" + preComputedToString())
    }
  }
}

/**
  * class which contains values updated by digestUpdates
  * @param updatedBij bijection modify by last changes
  * @param vehicleSearcher vehicle searcher modified by last changes
  * @param checkpoint0Defined if a checkpoint of level 0L was defined
  * @param stackDone if the current bijection is a stacked bijection
  * @param prevRoutes routes before the last change
  */
class UpdatedValues()

class ModifiedValues[@specialized(Long) U](val vehicle : Long,
                        val value : U,
                        val hasChangedSinceCheckpoint0 : Boolean){
  override def toString: String = {
    "ModifiedValues(" + vehicle + "," + value + "," + hasChangedSinceCheckpoint0 + ")"
  }
}

trait Segment[@specialized T]{}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was present in the global sequence when the pre-computation was performed
  * @param startNode the first node of the subsequence
  * @param startNodeValue the T value that the pre-computation associated with the node "startNode"
  * @param endNode the last node of the subsequence
  * @param endNodeValue the T value that the pre-computation associated with the node "endNode"
  * @tparam T the type of precomputation
  */
case class PreComputedSubSequence[@specialized T](startNode:Long,
                                                      startNodeValue:T,
                                                      endNode:Long,
                                                      endNodeValue:T) extends Segment[T]{
  override def toString: String = {
    "PreComputedSubSequence (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + ")"
  }
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was not present in the global sequence when the pre-computation was performed, but
  * the flippedd subsequence obtained by flippig it was present in the global sequence when the pre-computation was performed, but
  * @param startNode the first node of the subsequence (it was after the endNode when pre-computation ws performed)
  * @param startNodeValue the T value that the pre-computation associated with the node "startNode"
  * @param endNode the last node of the subsequence (it was before the endNode when pre-computation ws performed)
  * @param endNodeValue the T value that the pre-computation associated with the node "endNode"
  * @tparam T the type of precomputation
  */
case class FlippedPreComputedSubSequence[@specialized T](startNode:Long,
                                                             startNodeValue:T,
                                                             endNode:Long,
                                                             endNodeValue:T) extends Segment[T]{
  override def toString: String = {
    "FlippedPreComputedSubSequence (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + ")"
  }
}

/**
  * This represent that a node that was not present in the initial sequence when pre-computation was performed.
  * @param node
  */
case class NewNode[@specialized T](node:Long) extends Segment[T]{
  override def toString: String = {
    "NewNode - Node : " + node
  }
}
