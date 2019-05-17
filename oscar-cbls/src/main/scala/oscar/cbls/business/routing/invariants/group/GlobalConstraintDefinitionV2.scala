package oscar.cbls.business.routing.invariants.group

import oscar.cbls._
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.model.{RoutingConventionMethods, VehicleLocation}
import oscar.cbls.core._

import scala.collection.immutable.{HashMap, Stack}
import scala.collection.mutable

abstract class GlobalConstraintDefinitionV2 [T : Manifest, U:Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n = routes.maxValue+1L
  val vehicles = 0L until v

  val preComputedValues: Array[T] = new Array[T](n)
  val vehicleValues : Array[U] = new Array[U](v)
  var vehicleValuesAtLevel0 : Array[U] = new Array[U](v)
  var checkpointAtLevel0: IntSequence = _
  var changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, true)
  //var changedVehiclesSinceLastApplyUpdate = new IterableMagicBoolArray(v,false)
  var checkpointLevel: Int = -1
  val savedDataAtCheckPointLevel: mutable.Map[Int,(Option[Array[ListSegments]], VehicleLocation, IterableMagicBoolArray)] = new mutable.HashMap

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  registerStaticAndDynamicDependency(routes)

  finishInitialization()

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
                          segments:QList[Segment[T]],
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

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    //println("changes : " + changes)
    digestUpdates(changes) match{
      case None => for (vehicle <- vehicles){
        assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
      }
      case Some(x) if this.checkpointLevel == -1 =>
        for (vehicle <- vehicles){
          assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
        }
      case Some(x) =>
        changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(vehicle => {
          val segments = x(vehicle).segments
          vehicleValues(vehicle) = computeVehicleValue(vehicle, segments, changes.newValue, preComputedValues)
        })
    }
  }

  private def digestUpdates(changes:SeqUpdate): Option[Array[ListSegments]] = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        val newRoute = changes.newValue
        val prevUpdateSegments = digestUpdates(prev)

        if(checkpointLevel == 0){
          checkpointAtLevel0 = newRoute
          changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(vehicle => {
            performPreCompute(vehicle,newRoute,preComputedValues)
            vehicleValuesAtLevel0(vehicle) = vehicleValues(vehicle)
          })
          changedVehiclesSinceCheckpoint0.all_=(false)
          vehicleSearcher = vehicleSearcher.regularize
        }

        // If false it means that we had an assign value update => not incremental => from scratch
        val segmentsAtCheckPoint = if (prevUpdateSegments.isEmpty || this.checkpointLevel < 0) {
          computeAndAssignVehiclesValueFromScratch(changes.newValue)
          Some(Array.tabulate(v)(vehicle => {
            val lastNodeOfVehicle =
              if(vehicle < v-1) newRoute.valueAtPosition(vehicleSearcher.startPosOfVehicle(vehicle+1)-1).get
              else newRoute.valueAtPosition(newRoute.size-1).get
            ListSegments(new QList[Segment[T]](PreComputedSubSequence(vehicle, preComputedValues(vehicle), lastNodeOfVehicle, preComputedValues(lastNodeOfVehicle))),vehicle)
          }))
        } else {
          prevUpdateSegments
        }

        this.checkpointLevel = checkpointLevel

        savedDataAtCheckPointLevel(checkpointLevel) = (segmentsAtCheckPoint,vehicleSearcher,changedVehiclesSinceCheckpoint0)
        segmentsAtCheckPoint



      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0L) {
          require(checkpoint quickEquals this.checkpointAtLevel0)
          for(vehicle <- 0 until v) vehicleValues(vehicle) = vehicleValuesAtLevel0(vehicle)
        }
        val (segments, vehicleSearcherAtCheckpointLevel, changedVehiclesSinceCheckpoint) = savedDataAtCheckPointLevel(checkpointLevel)
        vehicleSearcher = vehicleSearcherAtCheckpointLevel
        changedVehiclesSinceCheckpoint0 = changedVehiclesSinceCheckpoint
        segments

      case sui@SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get
          val newPrevUpdateSegments = Array.tabulate(v)(lastPrevUpdateSegments)

          vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true

          // InsertSegment insert a segment AFTER a defined position and SeqUpdateInsert at a position => we must withdraw 1
          newPrevUpdateSegments(impactedVehicle) = newPrevUpdateSegments(impactedVehicle).insertSegments(QList[Segment[T]](NewNode(value).asInstanceOf[Segment[T]]),pos-1,prevRoutes)
          Some(newPrevUpdateSegments)
        }

      case sum@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get
          val newPrevUpdateSegments = Array.tabulate(v)(lastPrevUpdateSegments)

          val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
          val toVehicle = vehicleSearcher.vehicleReachingPosition(after)
          vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(fromVehicle) = true
          changedVehiclesSinceCheckpoint0(toVehicle) = true

          val (newFromVehicleListSegment, removedSegments) = newPrevUpdateSegments(fromVehicle).removeSegments(fromIncluded, toIncluded, prevRoutes)
          newPrevUpdateSegments(fromVehicle) = newFromVehicleListSegment
          if(flip)
            newPrevUpdateSegments(toVehicle) = newPrevUpdateSegments(toVehicle).insertSegments(removedSegments.qMap(_.flip).reverse, after, prevRoutes)
          else
            newPrevUpdateSegments(toVehicle) = newPrevUpdateSegments(toVehicle).insertSegments(removedSegments, after, prevRoutes)
          Some(newPrevUpdateSegments)
        }

      case sur@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get
          val newPrevUpdateSegments = Array.tabulate(v)(lastPrevUpdateSegments)

          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)
          vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true

          newPrevUpdateSegments(impactedVehicle) = newPrevUpdateSegments(impactedVehicle).removeSegments(position,position,prevRoutes)._1

          Some(newPrevUpdateSegments)
        }

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        //changedVehiclesSinceLastApplyUpdate.all_=(false)
        val vehiclesListSegments: Array[ListSegments] = if(savedDataAtCheckPointLevel.keys.isEmpty) Array.fill(v)(null) else savedDataAtCheckPointLevel(0)._1.get
        changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(vehicle => {
          val lastNodeOfVehicle =
            if(vehicle < v-1) value.valueAtPosition(vehicleSearcher.startPosOfVehicle(vehicle+1)-1).get
            else value.valueAtPosition(value.size-1).get

          vehiclesListSegments(vehicle) = ListSegments(QList[Segment[T]](PreComputedSubSequence(vehicle, preComputedValues(vehicle), lastNodeOfVehicle, preComputedValues(lastNodeOfVehicle))),vehicle)
        })
        Some(vehiclesListSegments)

      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  private def computeAndAssignVehiclesValueFromScratch(newSeq: IntSequence): Unit ={
    vehicles.foreach(v => assignVehicleValue(v, computeVehicleValueFromScratch(v, newSeq)))
  }

  private def preComputedToString():String = {
    "[" + preComputedValues.indices.map(i => "\n\t" + i + ":" + preComputedValues(i)).mkString("") + "\n]"
  }

  override def checkInternals(c : Checker): Unit = {
    for (v <- vehicles){
      require(computeVehicleValueFromScratch(v,routes.value).equals(vehicleValues(v)),
        "For Vehicle " + v + " : " + computeVehicleValueFromScratch(v,routes.value) + " " +
          vehicleValues(v) + " " + routes + "\n" + preComputedToString())
    }
  }



  case class ListSegments(segments: QList[Segment[T]], vehicle: Int){
    /**
      * Remove Segments and Sub-Segment from the Segments List
      *
      * Find the Segments holding the specified positions and split them in two parts right after these positions.
      * If a position is at the end of a Segment, this Segment is not splitted.
      * The in-between segments are gathered as well
      * @param from From position
      * @param to To position
      * @return A ListSegments containing the segments containing or between from and to
      */
    def removeSegments(from: Int, to: Int, routes: IntSequence): (ListSegments, QList[Segment[T]]) ={
      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment) =
        findImpactedSegment(from,routes)
      val (toImpactedSegment, segmentsBeforeToImpactedSegment, segmentsAfterToImpactedSegment) =
        findImpactedSegment(to,routes,QList(fromImpactedSegment,segmentsAfterFromImpactedSegment))
      // nodeBeforeFrom is always defined because it's at worst a vehicle node
      val nodeBeforeFrom = routes.explorerAtPosition(from - 1).get.value
      val fromNode = routes.explorerAtPosition(from).get.value
      val toNode = routes.explorerAtPosition(to).get.value
      val nodeAfterTo = routes.explorerAtPosition(to + 1)

      // 1° The removed segment is included in one segment
      if(fromImpactedSegment == toImpactedSegment){
        // The left-remaining part of the initial segment
        val (leftResidue: Option[Segment[T]], leftCuttedInitialSegment: Option[Segment[T]]) =
          fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode))
        // The right-remaining part of the initial segment
        val (removedSegment: Option[Segment[T]], rightResidue: Option[Segment[T]]) =
          if(nodeAfterTo.isDefined)
            leftCuttedInitialSegment.get.splitAtNode(toNode, nodeAfterTo.get.value, preComputedValues(toNode), preComputedValues(nodeAfterTo.get.value))
          else
            (leftCuttedInitialSegment, None)

        var newSegments: QList[Segment[T]] = segmentsAfterToImpactedSegment
        if(rightResidue.nonEmpty) {
          newSegments =
            if(newSegments == null)
              QList(rightResidue.get)
            else
              QList(rightResidue.get, newSegments)
        }
        if(leftResidue.nonEmpty) {
          newSegments =
            if(newSegments == null)
              QList(leftResidue.get)
            else
              QList(leftResidue.get, newSegments)
        }

        newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)

        (ListSegments(newSegments,vehicle), QList(removedSegment.get))
      } else {
        // The left-remaining part of the initial from segment
        val (leftResidue: Option[Segment[T]], leftCuttedFromSegment: Option[Segment[T]]) =
          fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode))
        // The right-remaining part of the initial to segment
        val (rightCuttedToSegment: Option[Segment[T]], rightResidue: Option[Segment[T]]) =
          if(nodeAfterTo.isDefined)
            toImpactedSegment.splitAtNode(toNode, nodeAfterTo.get.value, preComputedValues(toNode), preComputedValues(nodeAfterTo.get.value))
          else
            (toImpactedSegment, None)

        // We keep the segments between from impacted and to impacted

        var newSegments: QList[Segment[T]] = segmentsAfterToImpactedSegment
        if(rightResidue.nonEmpty) {
          newSegments =
            if(newSegments == null)
              QList(rightResidue.get)
            else
              QList(rightResidue.get, newSegments)
        }
        if(leftResidue.nonEmpty) {
          newSegments =
            if(newSegments == null)
              QList(leftResidue.get)
            else
              QList(leftResidue.get, newSegments)
        }

        newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)

        var removedSegments =
          QList(leftCuttedFromSegment.get,segmentsBeforeToImpactedSegment.tail)
        if(rightCuttedToSegment.nonEmpty)
          removedSegments = QList.nonReversedAppend(removedSegments,QList(rightCuttedToSegment.get))

        (ListSegments(newSegments,vehicle), removedSegments)
      }
    }

    /**
      * Insert a list of segments at the specified position
      * @param segmentsToInsert
      * @param afterPosition
      * @return
      */
    def insertSegments(segmentsToInsert: QList[Segment[T]], afterPosition: Int, routes: IntSequence): ListSegments ={
      val (impactedSegment, segmentsBeforeImpactedSegment, segmentsAfterImpactedSegment) = findImpactedSegment(afterPosition, routes)

      val insertAfterNode = routes.explorerAtPosition(afterPosition).get.value
      val insertBeforeNode = routes.explorerAtPosition(afterPosition+1)

      // We need to split the impacted segment in two and insert the new Segment between those two sub segment
      // If we are at the end of a vehicle route, we don't split the route
      val (leftResidue: Option[Segment[T]], rightResidue: Option[Segment[T]]) =
        if(insertBeforeNode.isDefined && insertBeforeNode.get.value >= v)
          impactedSegment.splitAtNode(insertAfterNode,insertBeforeNode.get.value,
            preComputedValues(insertAfterNode),preComputedValues(insertBeforeNode.get.value))
        else
          (Some(impactedSegment),None)

      var newSegments: QList[Segment[T]] = segmentsAfterImpactedSegment
      if(rightResidue.nonEmpty) {
        newSegments =
          if(newSegments == null)
            QList(rightResidue.get)
          else
            QList(rightResidue.get, newSegments)
      }
      newSegments = QList.nonReversedAppend(segmentsToInsert, newSegments)
      if(leftResidue.nonEmpty) {
        newSegments =
          if(newSegments == null)
            QList(leftResidue.get)
          else
            QList(leftResidue.get, newSegments)
      }

      newSegments = QList.nonReversedAppend(segmentsBeforeImpactedSegment, newSegments)

      ListSegments(newSegments, vehicle)
    }

    /**
      * This method finds the impacted segment of the previous update.
      * The segment is found if the endNode is after or equal to the search position.
      * It uses the previous route to get explorers and compare their position value to the searched position.
      *
      * WARNING : This implementation assumes that a good explorer cache has been implemented,
      *           otherwise it could be very slow. (Expecting to get the explorer in O(1)-time
      * @param pos the searched position
      * @param routes the used route for this search
      * @param currentSegments the list of segments of the impacted vehicle
      * @param exploredSegments the list of segments of that already have been checked
      * @return a tuple (impactedSegment: Segment[T], exploredSegments: Option[QList[Segment[T] ] ], unexploredSegments: Option[QList[Segment[T] ] ])
      */
    private def findImpactedSegment(pos: Int, routes: IntSequence,
                                    currentSegments: QList[Segment[T]] = segments,
                                    exploredSegments: QList[Segment[T]] = null): (Segment[T], QList[Segment[T]], QList[Segment[T]]) ={
      require(currentSegments.nonEmpty, "Segments list shouldn't be empty, if this append it means that the searched position isn't reach by this vehicle's segments")
      if(positionWithinSegment(pos, routes, currentSegments.head))
        (currentSegments.head,
          if(exploredSegments != null)exploredSegments.reverse
          else exploredSegments,
          currentSegments.tail)
      else
        findImpactedSegment(pos,routes,currentSegments.tail, QList(currentSegments.head, exploredSegments))
    }


    private def positionWithinSegment(pos: Int, routes: IntSequence, segment: Segment[T]): Boolean ={
      segment match{
        case PreComputedSubSequence(startNode,startNodeValue,endNode,endNodeValue) =>
          val endNodePos = routes.explorerAtAnyOccurrence(endNode).get.position
          endNodePos >= pos
        case FlippedPreComputedSubSequence(startNode,startNodeValue,endNode,endNodeValue) =>
          val endNodePos = routes.explorerAtAnyOccurrence(endNode).get.position
          endNodePos >= pos
        case NewNode(node) =>
          val nodePos = routes.explorerAtAnyOccurrence(node).get.position
          nodePos == pos
      }
    }

    override def toString: String ={
      "Segments of vehicle " + vehicle + " : " + segments.mkString(", ")
    }
  }

}

trait Segment[@specialized T]{
  /**
    * Split this Segment in two Segments right before the split node
    * If split node == start node, there will only be one Segment, the Segment itself
    * @return the left part of the splitted Segment (if exist) and the right part (starting at splitNode)
    */
  def splitAtNode(beforeSplitNode: Int, splitNode: Int, valueBeforeSplitNode: T, valueAtSplitNode: T): (Option[Segment[T]],Option[Segment[T]])

  def flip(): Segment[T]
}

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

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, valueBeforeSplitNode: T, valueAtSplitNode: T): (Option[Segment[T]],Option[Segment[T]]) = {
    if(splitNode == startNode) (None,Some(this))
    else if(beforeSplitNode == endNode) (Some(this),None)
    else {
      (Some(PreComputedSubSequence(startNode,startNodeValue,beforeSplitNode,valueBeforeSplitNode)),
        Some(PreComputedSubSequence(splitNode,valueAtSplitNode,endNode,endNodeValue)))
    }
  }

  override def flip(): Segment[T] = {
    FlippedPreComputedSubSequence(endNode, endNodeValue, startNode, startNodeValue)
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

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, valueBeforeSplitNode: T, valueAtSplitNode: T): (Option[Segment[T]],Option[Segment[T]]) = {
    if(splitNode == startNode) (None,Some(this))
    else if(beforeSplitNode == endNode) (Some(this),None)
    else {
      (Some(FlippedPreComputedSubSequence(startNode,startNodeValue,beforeSplitNode,valueBeforeSplitNode)),
        Some(FlippedPreComputedSubSequence(splitNode,valueAtSplitNode,endNode,endNodeValue)))
    }
  }

  override def flip(): Segment[T] = {
    PreComputedSubSequence(endNode, endNodeValue, startNode, startNodeValue)
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

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, valueBeforeSplitNode: T, valueAtSplitNode: T): (Option[Segment[T]],Option[Segment[T]]) = {
    require(beforeSplitNode == node)
    (Some(this), None)
  }

  override def flip(): Segment[T] = {
    this
  }
}
