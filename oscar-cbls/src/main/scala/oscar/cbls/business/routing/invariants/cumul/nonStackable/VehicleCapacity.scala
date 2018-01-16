package oscar.cbls.business.routing.invariants.cumul.nonStackable

import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.base.RoutingConventionMethods
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker


object VehicleCapacity{
  /**
    * the violation maintained by this invariant is, per vehicle,  the integral of overshoot over leaves, counted on nodeLeave
    * @param routes
    * @param v The numbers of vehicles
    * @param deltaAtNode Delta for each node. The initial content of a vehicle is the delta at its startong node
    * @param maxCapacity The maximal capacity shared by all vehicles
    * @param violation Violation of each vehicle; the integral of overshoot over nodes
    * @param contentAtEndOfVehicleRoute
    */
  @deprecated("Use VehicleCapacityV2", "We implement PreComputeInvariant")
  def apply(routes: ChangingSeqValue,
            v: Int,
            deltaAtNode: Array[Int],
            maxCapacity: Int,
            violation: Array[CBLSIntVar],
            contentAtEndOfVehicleRoute: Array[CBLSIntVar]): VehicleCapacity =
    new VehicleCapacity(routes, v, deltaAtNode, maxCapacity, violation, contentAtEndOfVehicleRoute)
}

@deprecated("Use VehicleCapacityV2", "We implement PreComputeInvariant")
class VehicleCapacity(routes:ChangingSeqValue,
                      v:Int,
                      deltaAtNode:Array[Int],
                      maxCapacity:Int,
                      violation:Array[CBLSIntVar],
                      contentAtEndOfVehicleRoute:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget{

  val overApproximatingBoundForMaxCapacity = deltaAtNode.foldLeft(0)({case (acc,delta) => acc + math.abs(delta)})

  registerStaticAndDynamicDependency(routes)
  this.finishInitialization()
  violation.foreach(_.setDefiningInvariant(this))
  contentAtEndOfVehicleRoute.foreach(_.setDefiningInvariant(this))

  val n = routes.maxValue + 1
  val vehicles = 0 until v

  //single checkpoint
  var checkpoint:IntSequence = null
  val contentOutAtCheckpoint:Array[Int] = Array.fill(n)(0)
  val nodeToLevelToNumberOfReachOutForwardAtCheckpoint:Array[RedBlackTreeMap[Int]] = Array.fill(n)(null)

  val changedVehiclesSinceCheckpoint = new IterableMagicBoolArray(v,false)

  var violationsAtCheckpoint:Array[Int] = Array.fill(v)(0)
  val contentAtEndOfVehicleRouteAtCheckpoint:Array[Int] = Array.fill(v)(0)

  //this one must always be up-to-date!

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) (_,_) => 0 else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  computeAndAffectViolationsFromScratch(routes.value)

  def saveCheckpoint(newCheckpoint:IntSequence){
    checkpoint = newCheckpoint
    for(vehicle <- changedVehiclesSinceCheckpoint.indicesAtTrue){
      doPrecomputeAtCheckpoint(vehicle)
      violationsAtCheckpoint(vehicle) = violation(vehicle).newValue
      contentAtEndOfVehicleRouteAtCheckpoint(vehicle) = contentAtEndOfVehicleRoute(vehicle).newValue
    }
    changedVehiclesSinceCheckpoint.all = false
  }

  def restoreCheckpoint(){
    for(vehicle <- changedVehiclesSinceCheckpoint.indicesAtTrue){
      violation(vehicle) := violationsAtCheckpoint(vehicle)
      contentAtEndOfVehicleRoute(vehicle) := contentAtEndOfVehicleRouteAtCheckpoint(vehicle)
    }
    changedVehiclesSinceCheckpoint.all = false
  }

  def recordTouchedVehicleSinceCheckpoint(vehicle:Int){
    changedVehiclesSinceCheckpoint(vehicle) = true
  }

  def addToReachCount(level:Int,addTo:RedBlackTreeMap[Int]):RedBlackTreeMap[Int] = {
    addTo.insert(level,addTo.getOrElse(level,0) + 1)
  }

  /**
    * computes the integral on x of(toFunction(x) - fromFunction(x)) with x in [minValueIncluded, maxValueIncluded]
    * @param fromFunction
    * @param toFunction
    * @param minValueIncluded
    * @param maxValueIncluded
    * @return
    */
  def computeIntegralInBoundsAbove(fromFunction:RedBlackTreeMap[Int], toFunction:RedBlackTreeMap[Int], minValueIncluded:Int, maxValueIncluded:Int):Int = {
    //the integral goes from high to low values

    @inline
    def stepIntegral(nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]], nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
                     positionOfIntegrator:Int, width:Int, acc:Int,
                     deltaWidth:Int, pivotValue:Int):Int = {
      if(pivotValue > maxValueIncluded) {
        //this pivot is still above the integration box
        require(acc == 0)
        computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt,nextPositionOnToOpt, pivotValue, width + deltaWidth, acc)
      }else{
        //this pivot is below or equal to the maxValueIncluded
        val valueAboveIncluded = if(positionOfIntegrator > maxValueIncluded) maxValueIncluded else positionOfIntegrator
        if(pivotValue >= minValueIncluded) {
          //just a square added, and carry on
          computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt,nextPositionOnToOpt, pivotValue, width + deltaWidth,
            acc + (valueAboveIncluded - pivotValue) * width)
        }else{
          //add a square and finish
          acc + (valueAboveIncluded - minValueIncluded + 1) * width
        }
      }
    }


    def computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]],
                                            nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
                                            positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      (nextPositionOnFromOpt, nextPositionOnToOpt) match {
        case (None, None) =>
          if(positionOfIntegrator >= maxValueIncluded){
            acc + width * (maxValueIncluded - minValueIncluded + 1)
          }else if (positionOfIntegrator >= minValueIncluded){
            acc + width * (positionOfIntegrator - minValueIncluded + 1)
          }else{
            acc
          }
        case (Some(nextPositionOnFrom), None) =>
          stepIntegral(
            nextPositionOnFrom.prev, nextPositionOnToOpt, positionOfIntegrator, width, acc,
            -nextPositionOnFrom.value, nextPositionOnFrom.key)
        case (None, Some(nextPositionOnTo)) =>
          stepIntegral(
            nextPositionOnFromOpt, nextPositionOnTo.prev, positionOfIntegrator,width, acc,
            nextPositionOnTo.value, nextPositionOnTo.key)
        case (Some(nextPositionOnFrom), Some(nextPositionOnTo)) =>
          if(nextPositionOnFrom.key == nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.prev, nextPositionOnTo.prev, positionOfIntegrator, width, acc,
              nextPositionOnTo.value - nextPositionOnFrom.value,
              nextPositionOnTo.key)
          else if(nextPositionOnFrom.key < nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFromOpt, nextPositionOnTo.prev, positionOfIntegrator, width, acc,
              nextPositionOnTo.value, nextPositionOnTo.key)
          else
            stepIntegral(
              nextPositionOnFrom.prev, nextPositionOnToOpt, positionOfIntegrator, width, acc,
              -nextPositionOnFrom.value, nextPositionOnFrom.key)
      }
    }

    computeIntegralInBoundsOptFromOptTo(fromFunction.biggestPosition,toFunction.biggestPosition, Int.MaxValue, 0, 0)
  }


  /**
    * compute the area between maxValueIncluded + 1 and (toFunction(x) - fromFunction(x)) for x in [minValueIncluded, maxValueIncluded]
    * @param fromFunction
    * @param toFunction
    * @param minValueIncluded
    * @param maxValueIncluded
    * @return
    */

  def computeIntegralInBoundsUnder(fromFunction:RedBlackTreeMap[Int], toFunction:RedBlackTreeMap[Int], minValueIncluded:Int,maxValueIncluded:Int):Int = {
    // the integral goes from low to high values
    @inline
    def stepIntegral(nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]], nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
                     positionOfIntegrator:Int, width:Int, acc:Int,
                     deltaWidth:Int, pivotValue:Int):Int = {
      if (pivotValue < minValueIncluded){
        require(acc==0)
        computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt,nextPositionOnToOpt, pivotValue, width + deltaWidth, acc)
      } else{
        val valueUnderIncluded = if (positionOfIntegrator < minValueIncluded) minValueIncluded else positionOfIntegrator
        if (pivotValue <= maxValueIncluded){
          computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt, nextPositionOnToOpt, pivotValue, width + deltaWidth,
            acc + (pivotValue - valueUnderIncluded) * width)
        } else{
          acc + (maxValueIncluded - valueUnderIncluded + 1) * width
        }
      }
    }

    def computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]],
                                            nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
                                            positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      (nextPositionOnFromOpt, nextPositionOnToOpt) match {
        case (None, None) =>
          if(positionOfIntegrator <= minValueIncluded){
            acc + width * (maxValueIncluded - minValueIncluded + 1)
          }else if (positionOfIntegrator <= maxValueIncluded){
            acc + width * (maxValueIncluded - positionOfIntegrator + 1)
          }else{
            acc
          }
        case (Some(nextPositionOnFrom), None) =>
          stepIntegral(
            nextPositionOnFrom.next, nextPositionOnToOpt, positionOfIntegrator, width, acc,
            -nextPositionOnFrom.value, nextPositionOnFrom.key)
        case (None, Some(nextPositionOnTo)) =>
          stepIntegral(
            nextPositionOnFromOpt, nextPositionOnTo.next, positionOfIntegrator,width, acc,
            nextPositionOnTo.value, nextPositionOnTo.key)
        case (Some(nextPositionOnFrom), Some(nextPositionOnTo)) =>
          if(nextPositionOnFrom.key == nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.next, nextPositionOnTo.next, positionOfIntegrator, width, acc,
              nextPositionOnTo.value - nextPositionOnFrom.value,
              nextPositionOnTo.key)
          else if(nextPositionOnFrom.key < nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.next, nextPositionOnToOpt, positionOfIntegrator, width, acc,
              -nextPositionOnFrom.value, nextPositionOnFrom.key)
          else
            stepIntegral(
              nextPositionOnFromOpt, nextPositionOnTo.next, positionOfIntegrator, width, acc,
              nextPositionOnTo.value, nextPositionOnTo.key)
      }
    }

    computeIntegralInBoundsOptFromOptTo(fromFunction.smallestPosition,toFunction.smallestPosition, Int.MinValue, 0, 0)
  }

  def doPrecomputeAtCheckpoint(vehicle:Int){
    val explorerAtVehicleStart = checkpoint.explorerAtAnyOccurrence(vehicle).head
    contentOutAtCheckpoint(vehicle) = deltaAtNode(vehicle)
    nodeToLevelToNumberOfReachOutForwardAtCheckpoint(vehicle) = RedBlackTreeMap(List((deltaAtNode(vehicle),1)))

    var explorerOpt = explorerAtVehicleStart.next
    var prevNode = vehicle

    while(explorerOpt match{
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if(node >= v){
          //continuing the same vehicle

          contentOutAtCheckpoint(node) = contentOutAtCheckpoint(prevNode) + deltaAtNode(node)
          nodeToLevelToNumberOfReachOutForwardAtCheckpoint(node) = addToReachCount(
            contentOutAtCheckpoint(node),
            nodeToLevelToNumberOfReachOutForwardAtCheckpoint(prevNode))

          prevNode = node
          explorerOpt = explorer.next
          true
        }else{
          //at the next vehicle
          false
        }
    }){}
  }

  def computeAndAffectViolationsFromScratch(seq:IntSequence){
    for(vehicle <- vehicles){
      recordTouchedVehicleSinceCheckpoint(vehicle)
      val (viol,contentEnd) = computeViolationFromScratchNoPrecompute(seq, vehicle)
      violation(vehicle) := viol
      contentAtEndOfVehicleRoute(vehicle) := contentEnd
    }
  }

  /**
   *
   * @param seq
   * @param vehicle
   * @return (the violation of the vehicle; the content of the vehicle when returning to its home)
   */
  def computeViolationFromScratchNoPrecompute(seq:IntSequence,vehicle:Int):(Int,Int) = {
    var currentContent = deltaAtNode(vehicle)
    var viol = if(currentContent > maxCapacity) currentContent - maxCapacity // violation at start node
    else 0
    var explorerOpt = seq.explorerAtAnyOccurrence(vehicle).head.next

    while(explorerOpt match{
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if(node >= v){
          //continuing the same vehicle
          currentContent = currentContent + deltaAtNode(node)
          if(currentContent > maxCapacity){
            //overshoot, add to violation, 1 hop
            viol += (currentContent - maxCapacity)
          }
          explorerOpt = explorer.next
          true
        }else{
          //at the next vehicle
          false
        }
    }){}
    (viol,currentContent)
  }

  def computeViolationFromScratchOnSegment(seq:IntSequence,incomingContentAtFromNodeIncluded:Int,fromNodeIncluded:Int,toNodeIncluded:Int):Int = {
    var currentContent = incomingContentAtFromNodeIncluded
    var currentViolation = 0
    var explorer = seq.explorerAtAnyOccurrence(fromNodeIncluded).head
    while({
      val node = explorer.value
      currentContent += this.deltaAtNode(node)
      if(currentContent > maxCapacity){
        currentViolation += (currentContent - maxCapacity)
      }
      if(node != toNodeIncluded){
        explorer = explorer.next.head
        true
      } else false
    }){}

    currentViolation
  }

  def computeViolationFromScratchOnSegmentBackward(seq:IntSequence,outgoingContentAtToNodeIncluded:Int,fromNodeIncluded:Int,toNodeIncluded:Int):Int = {
    var currentContent = outgoingContentAtToNodeIncluded
    var currentViolation = 0
    var explorer = seq.explorerAtAnyOccurrence(toNodeIncluded).head
    while({
      val node = explorer.value
      if(currentContent > maxCapacity){
        currentViolation += (currentContent - maxCapacity)
      }
      currentContent -= this.deltaAtNode(node)
      if(node != fromNodeIncluded){
        explorer = explorer.prev.head
        true
      } else false
    }){}

    currentViolation
  }

  def computeContentFromScratchForward(seq:IntSequence, incomingContentAtKnownNode:Int, targetNode:Int, knownNode:Int):Int={
    var currentContent = incomingContentAtKnownNode
    var explorer = seq.explorerAtAnyOccurrence(knownNode).head.next.head
    while({
      val node = explorer.value
      currentContent += this.deltaAtNode(node)
      if(node != targetNode) {
        explorer = explorer.next.head
        true
      }
      else false

    }){}
    currentContent
  }

  def computeSumDeltaFromScratchOnSegment(seq:IntSequence, fromValueIncluded:Int, toValueIncluded:Int):Int = {
    var currentSum = 0
    var explorer = seq.explorerAtAnyOccurrence(fromValueIncluded).head
    while({
      val node = explorer.value
      currentSum += this.deltaAtNode(node)
      if(node != toValueIncluded){
        explorer = explorer.next.head
        true
      }
      else false
    }){}
    currentSum
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes)) {
      for(v <- 0 until this.v) recordTouchedVehicleSinceCheckpoint(v)
      computeAndAffectViolationsFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate, isActive:Boolean, checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            computeAndAffectViolationsFromScratch(changes.newValue)
          }
          saveCheckpoint(changes.newValue)
          true
        }else{
          digestUpdates(prev)
        }
      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpoint)

          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }
      //TODO: handle the double Insert by supporting deltas on precomputations
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false
        val newSeq = changes.newValue
        //on which vehicle did we insert?

        val vehicle = vehicleSearcher(newSeq, pos)
        val deltaAtInsertedNode = deltaAtNode(value)

        if(!changedVehiclesSinceCheckpoint(vehicle)){
          //we can evaluate incrementally
          recordTouchedVehicleSinceCheckpoint(vehicle)
          val nodeAfterInsertedPoint = RoutingConventionMethods.routingSuccPos2Val(pos,changes.newValue,v)
          val nodeBeforeInsertedPoint = RoutingConventionMethods.routingPredPos2Val(pos,changes.newValue,v)

          if(nodeAfterInsertedPoint < v){
            //inserted at end of vehicle route
            assert(vehicle == nodeAfterInsertedPoint)

            val contentArrivingAtInsertedNode = contentOutAtCheckpoint(nodeBeforeInsertedPoint)
            val contentLeavingInsertedNode = contentArrivingAtInsertedNode + deltaAtNode(value)
            if(contentLeavingInsertedNode > maxCapacity){
              violation(vehicle) :+= (contentLeavingInsertedNode - maxCapacity)
            }

          }
          else{
            //somewhere in the vehicle route
            val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)
            val integralFrom = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(nodeBeforeInsertedPoint)
            val integralTo = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfVehicleRoute)

            //this is also the old content when getting at nodeAfterInsertionPoint
            val contentArrivingAtInsertedNode = contentOutAtCheckpoint(nodeBeforeInsertedPoint)
            val contentLeavingInsertedNode = contentArrivingAtInsertedNode + deltaAtInsertedNode

            val gainOnIntegralOnSegmentRemainAfterInsert =
              if (deltaAtInsertedNode == 0) 0
              else if (deltaAtInsertedNode > 0)
                // we insert a node with a delta > 0. So the violation of the remain segment increase
                computeIntegralInBoundsAbove(integralFrom,integralTo, maxCapacity - deltaAtInsertedNode + 1, maxCapacity)
              else
                // we insert a node with a delta < 0. So the violation of the remain segment decrease
                - computeIntegralInBoundsAbove(integralFrom,integralTo, maxCapacity + 1, maxCapacity - deltaAtInsertedNode)


            val violationAtInsertedNode =
              if(contentLeavingInsertedNode <= maxCapacity) 0
              else contentLeavingInsertedNode - maxCapacity

            val deltaOnViolation = gainOnIntegralOnSegmentRemainAfterInsert + violationAtInsertedNode
            violation(vehicle) :+= deltaOnViolation
          }

        }
        else{
          // need to use from scratch procedure :(

          val nodeAfterInsertedPoint = RoutingConventionMethods.routingSuccPos2Val(pos,changes.newValue,v)
          val lastPointOfVehicleRoutedBeforeInsert = RoutingConventionMethods.routingPredVal2Val(vehicle, prev.newValue, v)
          val lastPointOfVehicleRouteAfterInsert = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)

          val violationOnSegmentRemainIncludingInsertedNodeBeforeInsert =   if(violation(vehicle).newValue == 0) 0
          else  computeViolationFromScratchOnSegmentBackward(prev.newValue,contentAtEndOfVehicleRoute(vehicle).newValue,nodeAfterInsertedPoint,lastPointOfVehicleRoutedBeforeInsert)

          val violationOnSegmentRemainIncludingInsertedNodeAfterInsert =
            if(value == lastPointOfVehicleRouteAfterInsert)
              computeViolationFromScratchOnSegmentBackward(changes.newValue,contentAtEndOfVehicleRoute(vehicle).newValue + deltaAtInsertedNode, nodeAfterInsertedPoint, lastPointOfVehicleRouteAfterInsert)
            else
              computeViolationFromScratchOnSegmentBackward(changes.newValue,contentAtEndOfVehicleRoute(vehicle).newValue + deltaAtInsertedNode, value, lastPointOfVehicleRouteAfterInsert)

          val deltaOnViolation = violationOnSegmentRemainIncludingInsertedNodeAfterInsert - violationOnSegmentRemainIncludingInsertedNodeBeforeInsert
          violation(vehicle) :+= deltaOnViolation
        }

        contentAtEndOfVehicleRoute(vehicle) :+= deltaAtInsertedNode
        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) return false
        else if(x.isNop) return true
        else if(x.isSimpleFlip){

          val vehicle = vehicleSearcher(prev.newValue, fromIncluded)

          if(changedVehiclesSinceCheckpoint(vehicle)){
            //from scratch procedure
            val fromValue = x.fromValue
            val toValue = x.toValue
            //val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicle, prev.newValue, v)
            val contentAtEndOfSegment =
              computeContentFromScratchForward(prev.newValue, deltaAtNode(vehicle), toValue, vehicle)
            val violationOnSegmentBeforeFlip =
              computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtEndOfSegment, fromValue, toValue)

            val violationOnSegmentAfterFlip =
              computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndOfSegment, toValue, fromValue)

            val deltaOnViolation = violationOnSegmentAfterFlip - violationOnSegmentBeforeFlip
            violation(vehicle) :+= deltaOnViolation

          }
          else{
            //use pre-computation
            recordTouchedVehicleSinceCheckpoint(vehicle)
            val toValue = x.toValue
            val afterValue = x.afterValue
            val contentAtToValue = contentOutAtCheckpoint(afterValue) // it's a simple flip so after+1 == fromIncluded
            val contentAtToIncludedNode = contentOutAtCheckpoint(toValue)
            val h = contentAtToValue + contentAtToIncludedNode - maxCapacity

            val nodeBeforeToIncluded = RoutingConventionMethods.routingPredPos2Val(toIncluded, prev.newValue, v)
            val nodeBeforeAfter = RoutingConventionMethods.routingPredPos2Val(after, prev.newValue, v)

            val integralFromBeforeFlip = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(afterValue)
            val integralToBeforeFlip = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(toValue)

            val integralFromAfterFlip =
              if(afterValue < v) RedBlackTreeMap.empty[Int] // if after is start node
              else nodeToLevelToNumberOfReachOutForwardAtCheckpoint(nodeBeforeAfter)
            val integralToAfterFlip = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(nodeBeforeToIncluded)

            val violationOnSegmentBeforeFlip =
              computeIntegralInBoundsAbove(integralFromBeforeFlip, integralToBeforeFlip, maxCapacity +1, integralToBeforeFlip.biggestPosition.get.key)

            val violationOnSegmentAfterFlip =
              computeIntegralInBoundsUnder(integralFromAfterFlip, integralToAfterFlip, integralToAfterFlip.smallestPosition.get.key, h-1)

            val deltaOnViolation = violationOnSegmentAfterFlip - violationOnSegmentBeforeFlip

            violation(vehicle) :+= deltaOnViolation
          }

        }
        else {
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


          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue, toIncluded))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //the segment is moved to the same vehicle, so we do not consider node cost here

            if(changedVehiclesSinceCheckpoint(vehicleOfMovedSegment)){
              // from scratch procedure
              val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicleOfMovedSegment, prev.newValue, v)

              if(x.moveUpwards){

                val contentAtAfter =
                  computeContentFromScratchForward(prev.newValue, deltaAtNode(vehicleOfMovedSegment), afterValue, vehicleOfMovedSegment)
                // total segment is here the segment fromValue -> ... -> afterValue
                val violationOnTotalSegmentBeforeMove =
                    computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtAfter, fromValue, afterValue)

                val violationOnTotalSegmentAfterMove =
                  if(flip)
                    computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtAfter, oldSuccToValue, fromValue)
                  else
                    computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtAfter, oldSuccToValue, toValue)

                val deltaOnViolation = violationOnTotalSegmentAfterMove - violationOnTotalSegmentBeforeMove

                violation(vehicleOfMovedSegment) :+= deltaOnViolation
              }
              else{
                val contentAtToValue =
                  computeContentFromScratchForward(prev.newValue, deltaAtNode(vehicleOfMovedSegment), toValue, vehicleOfMovedSegment)

                // total segment is here the segment oldSuccAfterValue -> ... -> toValue
                val violationOnTotalSegmentBeforeMove =
                  computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtToValue, oldSuccAfterValue, toValue)

                val violationOnTotalSegmentAfterMove =
                  if(flip)
                    computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtToValue, toValue, oldPrevFromValue)
                  else
                    computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtToValue, fromValue, oldPrevFromValue)

                val deltaOnViolation = violationOnTotalSegmentAfterMove - violationOnTotalSegmentBeforeMove

                violation(vehicleOfMovedSegment) :+= deltaOnViolation
              }

            }
            else {

              recordTouchedVehicleSinceCheckpoint(vehicleOfMovedSegment)

              val contentAtAfterValue = contentOutAtCheckpoint(afterValue)
              val contentAtToValue = contentOutAtCheckpoint(toValue)
              val contentAtOldPrevFromValue = contentOutAtCheckpoint(oldPrevFromValue)

              val prevToValue = prev.newValue.valueAtPosition(toIncluded -1).head
              val oldPrevPrevFromValue = if(oldPrevFromValue < v) vehicleOfMovedSegment
              else  prev.newValue.valueAtPosition(fromIncluded-2).head

              val h = contentAtOldPrevFromValue + contentAtToValue - maxCapacity

              if(x.moveUpwards){

                // afterSegment is here the segment oldSuccToValue -> ... -> afterValue

                val sumDeltaOnSegment = contentAtToValue - contentAtOldPrevFromValue
                val sumDeltaOnAfterSegment = contentAtAfterValue - contentAtToValue

                val integralOldPrevFromValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevFromValue)
                val integralToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(toValue)
                val integralAfterValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(afterValue)
                val integralPrevToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(prevToValue)
                val integralPrevPrevFromValue =
                  if(oldPrevFromValue < v) RedBlackTreeMap.empty[Int]
                  else nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevPrevFromValue)

                val gainOnIntegralOnSegment =
                  if(!flip) {
                    if (sumDeltaOnAfterSegment == 0) 0
                    else if (sumDeltaOnAfterSegment > 0)
                    // if sumDeltaOnAfterSegment > 0, after the move the violation on moved segment increase
                      computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity - sumDeltaOnAfterSegment + 1, maxCapacity)
                    else
                    // if sumDeltaOnAfterSegment < 0, after the move the violation on moved segment decrease
                      -computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity + 1, maxCapacity - sumDeltaOnAfterSegment)
                  }
                  else{
                    (computeIntegralInBoundsUnder(integralPrevPrevFromValue, integralPrevToValue, integralPrevToValue.smallestPosition.get.key, h + sumDeltaOnAfterSegment -1)
                      - computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity+1, integralToValue.biggestPosition.get.key))
                  }

                val gainOnIntegralOnAfterSegment =
                  if(sumDeltaOnSegment == 0) 0
                  else if(sumDeltaOnSegment < 0)
                  // if sumDeltaOnSegment < 0, after the move the violation on afterSegment increase
                    computeIntegralInBoundsAbove(integralToValue, integralAfterValue, maxCapacity + sumDeltaOnSegment +1, maxCapacity)
                  else
                  // if if sumDeltaOnSegment > 0, after the move the violation on afterSegment decrease
                    - computeIntegralInBoundsAbove(integralToValue, integralAfterValue, maxCapacity + 1, maxCapacity + sumDeltaOnSegment)

                val deltaOnViolation = gainOnIntegralOnAfterSegment + gainOnIntegralOnSegment
                violation(vehicleOfMovedSegment) :+= deltaOnViolation
              }
              else{

                // after segment is here the segment oldSuccAfterValue -> ... -> oldPrevFromValue

                val sumDeltaonSegment = contentAtToValue - contentAtOldPrevFromValue
                val sumDeltaOnAfterSegment = contentAtOldPrevFromValue - contentAtAfterValue

                val integralAfterValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(afterValue)
                val integralOldPrevFromValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevFromValue)
                val integralToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(toValue)
                val integralPrevToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(prevToValue)
                val integralPrevPrevFromValue =
                  if(oldPrevFromValue < v) RedBlackTreeMap.empty[Int]
                  else nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevPrevFromValue)

                val gainOnIntegralOnSegment =
                  if(!flip) {
                    if (sumDeltaOnAfterSegment == 0) 0
                    else if (sumDeltaOnAfterSegment < 0)
                    // if sumDeltaOnAfterSegment < 0, after the move the violation on moved segment increase
                      computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity + sumDeltaOnAfterSegment + 1, maxCapacity)
                    else
                    // if sumDeltaOnAfterSegment > 0, after the move the violation on moved segment decrease
                      -computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity + 1, maxCapacity + sumDeltaOnAfterSegment)
                  }
                  else{
                    (computeIntegralInBoundsUnder(integralPrevPrevFromValue, integralPrevToValue, integralPrevToValue.smallestPosition.get.key, h - sumDeltaOnAfterSegment -1)
                      - computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity+1, integralToValue.biggestPosition.get.key))
                  }

                val gainOnIntegralOnAfterSegment =
                  if(sumDeltaonSegment == 0) 0
                  else if(sumDeltaonSegment > 0)
                  // if sumDeltaOnSegment > 0, after the move the violation on afterSegment increase
                    computeIntegralInBoundsAbove(integralAfterValue, integralOldPrevFromValue, maxCapacity - sumDeltaonSegment +1, maxCapacity)
                  else
                  // if sumDeltaOnSegment < 0, after the move the violation on afterSegment decrease
                    - computeIntegralInBoundsAbove(integralAfterValue, integralOldPrevFromValue, maxCapacity + 1, maxCapacity - sumDeltaonSegment)

                val deltaOnViolation = gainOnIntegralOnAfterSegment + gainOnIntegralOnSegment
                violation(vehicleOfMovedSegment) :+= deltaOnViolation
              }
            }

          }
          else{
            //moving a segment to another vehicle

            if(changedVehiclesSinceCheckpoint(vehicleOfMovedSegment)){
              // from scratch procedure

              //remove segment from vehicleOfMovedSegment
              val contentAtEndVehicleOfMovedSegmentBeforeMove = contentAtEndOfVehicleRoute(vehicleOfMovedSegment)
              val lastPointOfVehicleOfMovedSegment = RoutingConventionMethods.routingPredVal2Val(vehicleOfMovedSegment, prev.newValue, v)

              val sumDeltaOnMovedSegment = computeSumDeltaFromScratchOnSegment(prev.newValue, fromValue, toValue)

              val violationBeforeMoveOnVehicleOfMovedSegment = computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtEndVehicleOfMovedSegmentBeforeMove.newValue, fromValue, lastPointOfVehicleOfMovedSegment)
              val violationAfterMoveOnVehicleOfMovedSegment = if(toValue == lastPointOfVehicleOfMovedSegment) 0
              else computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndVehicleOfMovedSegmentBeforeMove.newValue - sumDeltaOnMovedSegment, oldSuccToValue, lastPointOfVehicleOfMovedSegment)

              val deltaOnViolationOnVehicleOfMovedSegment = violationAfterMoveOnVehicleOfMovedSegment - violationBeforeMoveOnVehicleOfMovedSegment

              violation(vehicleOfMovedSegment) :+= deltaOnViolationOnVehicleOfMovedSegment
              contentAtEndOfVehicleRoute(vehicleOfMovedSegment) :-= sumDeltaOnMovedSegment

              //insert segment to targetVehicle
              val contentAtEndTargetVehicle = contentAtEndOfVehicleRoute(targetVehicleOfMove)
              val lastPointOfTargetVehicle = RoutingConventionMethods.routingPredVal2Val(targetVehicleOfMove, prev.newValue, v)

              val violationBeforeMoveOnTargetVehicle = if(afterValue == lastPointOfTargetVehicle) 0
              else computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtEndTargetVehicle.newValue, oldSuccAfterValue, lastPointOfTargetVehicle)
              val violationAfterMoveOnTargetVehicle =
                if(! flip) {
                  if (afterValue == lastPointOfTargetVehicle) computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, fromValue, toValue)
                  else computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, fromValue, lastPointOfTargetVehicle)
                }
                else{
                  if (afterValue == lastPointOfTargetVehicle) computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, toValue, fromValue)
                  else computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, toValue, lastPointOfTargetVehicle)
                }

              val deltaOnViolationOnTargetVehicle = violationAfterMoveOnTargetVehicle - violationBeforeMoveOnTargetVehicle

              violation(targetVehicleOfMove) :+= deltaOnViolationOnTargetVehicle
              contentAtEndOfVehicleRoute(targetVehicleOfMove) :+= sumDeltaOnMovedSegment
            }
            else if(!changedVehiclesSinceCheckpoint(vehicleOfMovedSegment) && changedVehiclesSinceCheckpoint(targetVehicleOfMove)){
              // pre-compute for removing the segment on vehicle of moved segment and from scratch procedure for insert in target vehicle

              recordTouchedVehicleSinceCheckpoint(vehicleOfMovedSegment)

              val lastPointOfVehicleOfMovedSegment = RoutingConventionMethods.routingPredVal2Val(vehicleOfMovedSegment, prev.newValue, v)
              val contentAtToValue = contentOutAtCheckpoint(toValue)
              val contentAtOldPrevFromValue = contentOutAtCheckpoint(oldPrevFromValue)

              val sumDeltaOnMovedSegment = contentAtToValue - contentAtOldPrevFromValue

              val integralOldPrevFromValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevFromValue)
              val integralToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(toValue)
              val integralLastPointOfVehicleOfMovedSegment = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfVehicleOfMovedSegment)

              val violationOnSegmentBeforeMove = computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity +1, integralToValue.biggestPosition.get.key)
              val gainOnViolationOnRemainSegmentAfterMove =
                if (sumDeltaOnMovedSegment == 0) 0
                else if(sumDeltaOnMovedSegment > 0)
                  // if sumDeltaOnMovedSegment < 0, the violation on remain segment decrease
                  - computeIntegralInBoundsAbove(integralToValue, integralLastPointOfVehicleOfMovedSegment, maxCapacity + 1, maxCapacity + sumDeltaOnMovedSegment)
                else
                  // if sumDeltaOnMovedSegment > 0, the violation on remain segment increase
                  computeIntegralInBoundsAbove(integralToValue, integralLastPointOfVehicleOfMovedSegment, maxCapacity + sumDeltaOnMovedSegment + 1, maxCapacity)

              val deltaOnViolationOnVehicleOfMovedSegment = gainOnViolationOnRemainSegmentAfterMove - violationOnSegmentBeforeMove

              violation(vehicleOfMovedSegment) :+= deltaOnViolationOnVehicleOfMovedSegment
              contentAtEndOfVehicleRoute(vehicleOfMovedSegment) :-= sumDeltaOnMovedSegment

              // insert segment to targetVehicle
              val contentAtEndTargetVehicle = contentAtEndOfVehicleRoute(targetVehicleOfMove)
              val lastPointOfTargetVehicle = RoutingConventionMethods.routingPredVal2Val(targetVehicleOfMove, prev.newValue, v)

              val violationBeforeMoveOnTargetVehicle = if(afterValue == lastPointOfTargetVehicle) 0
              else computeViolationFromScratchOnSegmentBackward(prev.newValue, contentAtEndTargetVehicle.newValue, oldSuccAfterValue, lastPointOfTargetVehicle)
              val violationAfterMoveOnTargetVehicle =
                if(! flip) {
                  if (afterValue == lastPointOfTargetVehicle) computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, fromValue, toValue)
                  else computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, fromValue, lastPointOfTargetVehicle)
                }
                else{
                  if (afterValue == lastPointOfTargetVehicle) computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, toValue, fromValue)
                  else computeViolationFromScratchOnSegmentBackward(changes.newValue, contentAtEndTargetVehicle.newValue + sumDeltaOnMovedSegment, toValue, lastPointOfTargetVehicle)
                }

              val deltaOnViolationOnTargetVehicle = violationAfterMoveOnTargetVehicle - violationBeforeMoveOnTargetVehicle

              violation(targetVehicleOfMove) :+= deltaOnViolationOnTargetVehicle
              contentAtEndOfVehicleRoute(targetVehicleOfMove) :+= sumDeltaOnMovedSegment
            }
            else{
              // pre-compute

              recordTouchedVehicleSinceCheckpoint(vehicleOfMovedSegment)
              recordTouchedVehicleSinceCheckpoint(targetVehicleOfMove)

              //remove segment from vehicle of moved segment
              val lastPointOfVehicleOfMovedSegment = RoutingConventionMethods.routingPredVal2Val(vehicleOfMovedSegment, prev.newValue, v)
              val contentAtToValue = contentOutAtCheckpoint(toValue)
              val contentAtOldPrevFromValue = contentOutAtCheckpoint(oldPrevFromValue)

              val sumDeltaOnMovedSegment = contentAtToValue - contentAtOldPrevFromValue

              val integralOldPrevFromValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevFromValue)
              val integralToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(toValue)
              val integralLastPointOfVehicleOfMovedSegment = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfVehicleOfMovedSegment)

              val violationOnSegmentBeforeMove = computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity +1, integralToValue.biggestPosition.get.key)
              val gainOnViolationOnRemainSegmentAfterMove =
                if (sumDeltaOnMovedSegment == 0) 0
                else if(sumDeltaOnMovedSegment > 0)
                // if sumDeltaOnMovedSegment < 0, the violation on remain segment decrease
                  - computeIntegralInBoundsAbove(integralToValue, integralLastPointOfVehicleOfMovedSegment, maxCapacity + 1, maxCapacity + sumDeltaOnMovedSegment)
                else
                // if sumDeltaOnMovedSegment > 0, the violation on remain segment increase
                  computeIntegralInBoundsAbove(integralToValue, integralLastPointOfVehicleOfMovedSegment, maxCapacity + sumDeltaOnMovedSegment + 1, maxCapacity)

              val deltaOnViolationOnVehicleOfMovedSegment = gainOnViolationOnRemainSegmentAfterMove - violationOnSegmentBeforeMove

              violation(vehicleOfMovedSegment) :+= deltaOnViolationOnVehicleOfMovedSegment
              contentAtEndOfVehicleRoute(vehicleOfMovedSegment) :-= sumDeltaOnMovedSegment

              //insert segment
              val lastPointOfTargetVehicle = RoutingConventionMethods.routingPredVal2Val(targetVehicleOfMove, prev.newValue, v)
              val contentAtAfterValue = contentOutAtCheckpoint(afterValue)
              val oldPrevPrevFromValue = RoutingConventionMethods.routingPredVal2Val(oldPrevFromValue, prev.newValue, v)
              val oldPrevToValue = RoutingConventionMethods.routingPredVal2Val(toValue, prev.newValue, v)

              val integralAfterValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(afterValue)
              val integralLastPointOfTargetVehicle = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfTargetVehicle)
              val integralOldPrevPrevFromValue = if(oldPrevFromValue == vehicleOfMovedSegment) RedBlackTreeMap.empty[Int]
              else nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevPrevFromValue)
              val integralOldPrevToValue = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(oldPrevToValue)

              val violationOnMovedSegmentAfterMove =
                if (!flip)
                  computeIntegralInBoundsAbove(integralOldPrevFromValue, integralToValue, maxCapacity - contentAtAfterValue + contentAtOldPrevFromValue + 1 , integralToValue.biggestPosition.get.key)
                else
                  computeIntegralInBoundsUnder(integralOldPrevPrevFromValue, integralOldPrevToValue, integralOldPrevToValue.smallestPosition.get.key, contentAtAfterValue+contentAtToValue-maxCapacity-1)

              // afterSegment is here the segment afterValue+1 -> ... -> lastPointOfVehicle
              val gainOfViolationOnAfterSegment =
                if (afterValue == lastPointOfTargetVehicle) 0
                else if (sumDeltaOnMovedSegment == 0) 0
                else if (sumDeltaOnMovedSegment > 0)
                  // if sumDeltaOnMovedSegment > 0, the violation on afterSegment increase
                  computeIntegralInBoundsAbove(integralAfterValue, integralLastPointOfTargetVehicle, maxCapacity - sumDeltaOnMovedSegment+1, maxCapacity)
                else
                // if sumDeltaOnMovedSegment < 0, the violation on afterSegment decrease
                  - computeIntegralInBoundsAbove(integralAfterValue, integralLastPointOfTargetVehicle, maxCapacity+1, maxCapacity - sumDeltaOnMovedSegment)

              val deltaOnViolationOnTargetVehicle = gainOfViolationOnAfterSegment + violationOnMovedSegmentAfterMove

              violation(targetVehicleOfMove) :+= deltaOnViolationOnTargetVehicle
              contentAtEndOfVehicleRoute(targetVehicleOfMove) :+= sumDeltaOnMovedSegment
            }
          }
        }

        true
      case x@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev)) return false

        val oldSeq = prev.newValue
        val vehicle = vehicleSearcher(oldSeq, pos)
        val deltaAtRemovedNode = deltaAtNode(removedValue)

        if(!changedVehiclesSinceCheckpoint(vehicle)){
          //we can evaluate incrementally
          recordTouchedVehicleSinceCheckpoint(vehicle)
          val nodeAfterRemovedPoint = RoutingConventionMethods.routingSuccPos2Val(pos,changes.newValue,v)
          val contentAtRemovedNode = contentOutAtCheckpoint(removedValue)

          if(nodeAfterRemovedPoint < v){
            //removed at end of vehicle route
            assert(vehicle == nodeAfterRemovedPoint)

            if(contentAtRemovedNode > maxCapacity){
              violation(vehicle) :-= (contentAtRemovedNode - maxCapacity)

            }

          } else{
            //somewhere in the vehicle route

            val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)
            val integralFrom = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(removedValue)
            val integralTo = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfVehicleRoute)

            val gainOnIntegralOnSegmentRemainAfterRemove =
              if(deltaAtRemovedNode == 0) 0
              else if(deltaAtRemovedNode < 0)
                // we remove a node with a delta < 0. So the violation on the remain segment increase
               computeIntegralInBoundsAbove(integralFrom, integralTo, maxCapacity + deltaAtRemovedNode + 1, maxCapacity)
              else
                // we remove a node with a delta > 0. So the violation on the remain segment decrease
                - computeIntegralInBoundsAbove(integralFrom, integralTo, maxCapacity +1, maxCapacity + deltaAtRemovedNode)
            val violationAtRemovedNode =
              if(contentAtRemovedNode <= maxCapacity) 0
              else contentAtRemovedNode - maxCapacity

            val deltaOnViolation = gainOnIntegralOnSegmentRemainAfterRemove - violationAtRemovedNode
            violation(vehicle) :+= deltaOnViolation
          }

        } else{
          // need from scratch procedure

          val nodeAfterRemovedPoint = RoutingConventionMethods.routingSuccPos2Val(pos, prev.newValue,v)
          val lastPointOfVehicleRouteBeforeRemove = RoutingConventionMethods.routingPredVal2Val(vehicle, prev.newValue, v)
          val lastPointOfVehicleRouteAfterRemove = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)

          val violationOnSegmentRemainIncludingRemovedNodeBeforeRemove =   if(violation(vehicle).newValue == 0) 0
          else  computeViolationFromScratchOnSegmentBackward(prev.newValue,contentAtEndOfVehicleRoute(vehicle).newValue,removedValue,lastPointOfVehicleRouteBeforeRemove)

          val violationOnSegmentRemainIncludingRemovedNodeAfterRemove = if(removedValue == lastPointOfVehicleRouteBeforeRemove) 0
          else computeViolationFromScratchOnSegmentBackward(changes.newValue,contentAtEndOfVehicleRoute(vehicle).newValue - deltaAtRemovedNode,nodeAfterRemovedPoint,lastPointOfVehicleRouteAfterRemove)

          val deltaOnViolation = violationOnSegmentRemainIncludingRemovedNodeAfterRemove - violationOnSegmentRemainIncludingRemovedNodeBeforeRemove
          violation(vehicle) :+= deltaOnViolation
        }

        contentAtEndOfVehicleRoute(vehicle) :-= deltaAtRemovedNode
        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }


  override def checkInternals(c : Checker) : Unit = {
    for(v <- vehicles){
      val (viol,contentAtEnd) = computeViolationFromScratchNoPrecompute(routes.value,v)
      c.check(violation(v).value == viol, Some("Error on violation of vehicle " + v + " expected:" + viol + " actual output:" + violation(v).value))
      c.check(contentAtEndOfVehicleRoute(v).value == contentAtEnd, Some("Error on content at end of vehicle route v:" + v + " expected:" + contentAtEnd + " actual output:" + contentAtEndOfVehicleRoute(v).value))
    }
  }

  override def toString : String = {
    "VehicleCapacity(routes: " + routes.name + " n: " + n + " v: " + v + " maxCapacity: " + maxCapacity +"){\n" +
      (0 until v).toList.map((vehicle:Int) =>
      {
        val header = "\tvehicle" + vehicle + " contentAtVehicleEnd: " + contentAtEndOfVehicleRoute(vehicle).newValue + " totalViolation: "+ violation(vehicle).newValue +"\n"
        var explorerOpt = routes.value.explorerAtAnyOccurrence(vehicle).get.next
        var contentAtNode = deltaAtNode(vehicle)
        var acc:String = "\tnode: " + f"$vehicle%-7d" + "\t" + "deltaAtNode: " + f"${deltaAtNode(vehicle)}%-4d" + "\t"+ "contentAtNode: "+ f"$contentAtNode%-4d" + (if (contentAtNode > maxCapacity) "\t"+ "violation:" + (contentAtNode - maxCapacity) else "") + "\n"
        var currentRoute = Array(vehicle)
        while(explorerOpt match{
          case None => //at end of last vehicle
            false
          case Some(explorer) if explorer.value < v =>
            //reached another vehicle
            false
          case Some(explorer) if explorer.value >= v =>
            val node = explorer.value
            contentAtNode += deltaAtNode(node)
            acc += "\tnode: " + f"$node%-7d" + "\t" + "deltaAtNode: " + f"${deltaAtNode(node)}%-4d" + "\t" + "contentAtNode: "+ f"$contentAtNode%-4d" + (if (contentAtNode > maxCapacity) "\t" + "violation: " + (contentAtNode - maxCapacity) else "") + "\n"
            explorerOpt = explorer.next
            currentRoute = currentRoute :+ node
            true
        }){}
        header+"\troute: "+currentRoute.mkString("->")+"\n"+acc+"}\n"}).mkString("")
  }
}