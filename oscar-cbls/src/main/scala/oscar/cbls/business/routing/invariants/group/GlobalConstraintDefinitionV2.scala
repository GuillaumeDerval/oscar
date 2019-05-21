package oscar.cbls.business.routing.invariants.group

import oscar.cbls._
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick
import oscar.cbls.algo.quick.{QList, QListIterator}
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
          //changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(vehicle => {
          (0 until v).foreach(vehicle => {
            performPreCompute(vehicle,newRoute,preComputedValues)
            vehicleValuesAtLevel0(vehicle) = vehicleValues(vehicle)
          })
          changedVehiclesSinceCheckpoint0.all_=(false)
          vehicleSearcher = vehicleSearcher.regularize
        }

        // If false it means that we had an assign value update => not incremental => from scratch
        val segmentsAtCheckPoint = if (prevUpdateSegments.isEmpty || this.checkpointLevel < 0) {
          computeAndAssignVehiclesValueFromScratch(changes.newValue)
          Some(Array.tabulate(v)(vehicle => initSegmentsOfVehicle(vehicle,newRoute)))
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

          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos-1)

          // InsertSegment insert a segment AFTER a defined position and SeqUpdateInsert at a position => we must withdraw 1
          newPrevUpdateSegments(impactedVehicle) = newPrevUpdateSegments(impactedVehicle).insertSegments(QList[Segment[T]](NewNode(value).asInstanceOf[Segment[T]]),pos-1,prevRoutes)

          vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true

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

          // Identification of the sub-segments to remove
          val segmentsToRemove =
            newPrevUpdateSegments(fromVehicle).findSubSegmentsToRemove(fromIncluded, toIncluded, prevRoutes)
          // Insert the sub-segments at his new position
          val segmentsAfterInsertion =
            if(flip)
              newPrevUpdateSegments(toVehicle).insertSegments(segmentsToRemove.qMap(_.flip).reverse, after, prevRoutes)
            else
              newPrevUpdateSegments(toVehicle).insertSegments(segmentsToRemove, after, prevRoutes)

          newPrevUpdateSegments(toVehicle) = segmentsAfterInsertion

          // Remove the sub-segments from his old position
          val segmentsAfterRemoval = newPrevUpdateSegments(fromVehicle).removeSegments(segmentsToRemove, prevRoutes, fromIncluded, toIncluded)
          newPrevUpdateSegments(fromVehicle) = segmentsAfterRemoval

          vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(fromVehicle) = true
          changedVehiclesSinceCheckpoint0(toVehicle) = true

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

          val segmentsToRemove = newPrevUpdateSegments(impactedVehicle).findSubSegmentsToRemove(position,position,prevRoutes)

          newPrevUpdateSegments(impactedVehicle) =
            newPrevUpdateSegments(impactedVehicle).removeSegments(segmentsToRemove, prevRoutes, position, position)

          vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true

          Some(newPrevUpdateSegments)
        }

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        val vehiclesListSegments: Array[ListSegments] = if(savedDataAtCheckPointLevel.keys.isEmpty) Array.fill(v)(null) else savedDataAtCheckPointLevel(0)._1.get
        (0 until v).foreach(vehicle => vehiclesListSegments(vehicle) = initSegmentsOfVehicle(vehicle, value))
        Some(vehiclesListSegments)

      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  private def initSegmentsOfVehicle(vehicle: Int, route: IntSequence): ListSegments ={
    val posOfVehicle = vehicleSearcher.startPosOfVehicle(vehicle)
    val (lastNodeOfVehicle, posOfLastNodeOfVehicle) =
      if(vehicle < v-1) {
        val lastNodePos = vehicleSearcher.startPosOfVehicle(vehicle+1)-1
        (route.valueAtPosition(lastNodePos).get,lastNodePos)
      }
      else {
        (route.valueAtPosition(route.size-1).get,route.size-1)
      }


    ListSegments(
      QList[Segment[T]](PreComputedSubSequence(
        vehicle,
        preComputedValues(vehicle),
        lastNodeOfVehicle,
        preComputedValues(lastNodeOfVehicle),
        posOfLastNodeOfVehicle - posOfVehicle + 1
      )),
      vehicle)
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
    private val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)
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
    def findSubSegmentsToRemove(from: Int, to: Int, routes: IntSequence): (QList[Segment[T]]) ={
      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment, currentCounter) =
        findImpactedSegment(from,vehicle)
      val (toImpactedSegment, segmentsBetweenFromAndTo, segmentsAfterToImpactedSegment,_) =
        findImpactedSegment(to,vehicle,QList(fromImpactedSegment,segmentsAfterFromImpactedSegment), initCounter = Some(currentCounter))
      // nodeBeforeFrom is always defined because it's at worst a vehicle node
      val beforeFromExplorer = routes.explorerAtPosition(from -1).get
      val toExplorer = routes.explorerAtPosition(to).get

      val nodeBeforeFrom = beforeFromExplorer.value
      val fromNode = beforeFromExplorer.next.get.value
      val toNode = toExplorer.value
      val nodeAfterTo = toExplorer.next

      val lengthUntilFromImpactedSegment =
        QList.qFold[Segment[T],Long](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0)
      val lengthToFromImpactedSegment = lengthUntilFromImpactedSegment + fromImpactedSegment.length()
      val lengthToToImpactedSegment =
        QList.qFold[Segment[T],Long](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0) +
          QList.qFold[Segment[T],Long](segmentsBetweenFromAndTo, (acc,item) => acc + item.length(),0) +
          toImpactedSegment.length()

      // 1° The removed segment is included in one segment
      if(fromImpactedSegment == toImpactedSegment){
        val fromLeftResidueLength = from - lengthUntilFromImpactedSegment - vehiclePos
        val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength

        val toRightResidueLength =
          if (nodeAfterTo.isEmpty) 0
          else fromRightResidueLength - to + from - 1
        val toLeftResidueLength =
          fromRightResidueLength - toRightResidueLength

        // The left-remaining part of the initial segment
        val (_, leftCuttedInitialSegment: Option[Segment[T]]) =
          fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode), fromLeftResidueLength, fromRightResidueLength)
        // The right-remaining part of the initial segment
        val (removedSegment: Option[Segment[T]], _) =
          if(nodeAfterTo.isDefined)
            leftCuttedInitialSegment.get.splitAtNode(toNode, nodeAfterTo.get.value, preComputedValues(toNode), preComputedValues(nodeAfterTo.get.value), toLeftResidueLength, toRightResidueLength)
          else
            (leftCuttedInitialSegment, None)
        QList(removedSegment.get)
      } else {
        val toRightResidueLength =
          if(nodeAfterTo.isEmpty) 0
          else lengthToToImpactedSegment - to + vehiclePos
        val toLeftResidueLength =
          lengthToToImpactedSegment - toRightResidueLength

        val fromRightResidueLength = lengthToFromImpactedSegment - from + vehiclePos
        val fromLeftResidueLength = lengthToFromImpactedSegment - fromRightResidueLength

        // The left-remaining part of the initial from segment
        val (_, leftCuttedFromSegment: Option[Segment[T]]) =
          fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode), fromLeftResidueLength, fromRightResidueLength)
        // The right-remaining part of the initial to segment
        val (rightCuttedToSegment: Option[Segment[T]], _) =
          if(nodeAfterTo.isDefined)
            toImpactedSegment.splitAtNode(toNode, nodeAfterTo.get.value, preComputedValues(toNode), preComputedValues(nodeAfterTo.get.value), toLeftResidueLength, toRightResidueLength)
          else
            (toImpactedSegment, None)

        var removedSegments =
          QList(leftCuttedFromSegment.get,segmentsBetweenFromAndTo.tail)
        if(rightCuttedToSegment.nonEmpty)
          removedSegments = QList.nonReversedAppend(removedSegments,QList(rightCuttedToSegment.get))

        removedSegments
      }
    }

    /**
      * Insert a list of segments at the specified position
      * @param segmentsToInsert
      * @param afterPosition
      * @return
      */
    def insertSegments(segmentsToInsert: QList[Segment[T]], afterPosition: Int, routes: IntSequence): ListSegments ={
      val (impactedSegment, segmentsBeforeImpactedSegment, segmentsAfterImpactedSegment,_) = findImpactedSegment(afterPosition, vehicle)

      val insertAfterExplorer = routes.explorerAtPosition(afterPosition).get
      val insertAfterNode = insertAfterExplorer.value
      val insertBeforeNode = insertAfterExplorer.next

      val rightResidueLength =
        if(insertBeforeNode.isEmpty) 0
        else
          QList.qFold[Segment[T],Long](segmentsBeforeImpactedSegment, (acc,item) => acc + item.length(),0) +
            impactedSegment.length() - insertBeforeNode.get.position + vehiclePos
      val leftResidueLength = impactedSegment.length() - rightResidueLength

      // We need to split the impacted segment in two and insert the new Segment between those two sub segment
      // If we are at the end of a vehicle route, we don't split the route
      val (leftResidue: Option[Segment[T]], rightResidue: Option[Segment[T]]) =
        if(insertBeforeNode.isDefined && insertBeforeNode.get.value >= v)
          impactedSegment.splitAtNode(insertAfterNode,insertBeforeNode.get.value,
            preComputedValues(insertAfterNode),preComputedValues(insertBeforeNode.get.value),leftResidueLength, rightResidueLength)
        else
          (Some(impactedSegment),None)

      var newSegments: QList[Segment[T]] = segmentsAfterImpactedSegment
      if(rightResidue.nonEmpty) {
        newSegments = QList(rightResidue.get, newSegments)
      }
      newSegments = QList.nonReversedAppend(segmentsToInsert, newSegments)
      if(leftResidue.nonEmpty) {
        newSegments = QList(leftResidue.get, newSegments)
      }

      newSegments = QList.nonReversedAppend(segmentsBeforeImpactedSegment, newSegments)

      ListSegments(newSegments, vehicle)
    }


    /**
      * This method replace some segments by newSegments.
      *
      * The startNode of  the first segment of "newSegments" and
      * the endNode of the last segment of "newSegments" have to be part of segments.
      *
      * It will replace a list of segments of "segments" starting at startNode end ending at endNode with newSegments.
      * @return
      */
    def removeSegments(segmentsToRemove: QList[Segment[T]], routes: IntSequence, from: Long, to: Long): ListSegments ={
      val lastSegment = segmentsToRemove.last
      val firstSegment = segmentsToRemove.head

      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment, currentCounter) =
        findImpactedSegment(from, vehicle)
      val (toImpactedSegment, segmentsBetweenFromAndTo, segmentsAfterToImpactedSegment,_) =
        findImpactedSegment(to, vehicle, QList(fromImpactedSegment,segmentsAfterFromImpactedSegment), initCounter = Some(currentCounter))

      val nodeBeforeFrom = routes.explorerAtPosition(from - 1).get.value
      val nodeAfterTo = routes.explorerAtPosition(to + 1)

      val lengthUntilFromImpactedSegment = QList.qFold[Segment[T],Long](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0)
      val lengthToFromImpactedSegment = lengthUntilFromImpactedSegment + fromImpactedSegment.length()
      val lengthToToImpactedSegment =
        QList.qFold[Segment[T],Long](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0) +
        QList.qFold[Segment[T],Long](segmentsBetweenFromAndTo, (acc,item) => acc + item.length(),0) +
        toImpactedSegment.length()

      val(fromLeftResidueLength, fromRightResidueLength, toLeftResidueLength, toRightResidueLength) = if(lastSegment == firstSegment) {
        val fromLeftResidueLength = from - lengthUntilFromImpactedSegment - vehiclePos
        val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength

        val toRightResidueLength =
          if (nodeAfterTo.isEmpty) 0
          else fromRightResidueLength - to + from - 1
        val toLeftResidueLength =
          fromRightResidueLength - toRightResidueLength

        (fromLeftResidueLength,fromRightResidueLength,toLeftResidueLength,toRightResidueLength)
      } else {
        val toRightResidueLength =
          if (nodeAfterTo.isEmpty) 0
          else lengthToToImpactedSegment - to + vehiclePos
        val toLeftResidueLength =
          lengthToToImpactedSegment - toRightResidueLength
        val fromRightResidueLength = lengthToFromImpactedSegment - from + vehiclePos
        val fromLeftResidueLength = lengthToFromImpactedSegment - fromRightResidueLength
        (fromLeftResidueLength,fromRightResidueLength,toLeftResidueLength,toRightResidueLength)
      }

      val (leftResidue, _) =
        fromImpactedSegment.splitAtNode(nodeBeforeFrom, firstSegment.startNode(),
          preComputedValues(nodeBeforeFrom), preComputedValues(firstSegment.startNode()), fromLeftResidueLength, fromRightResidueLength)
      val (_, rightResidue) =
        if(nodeAfterTo.isDefined)
          toImpactedSegment.splitAtNode(lastSegment.endNode(), nodeAfterTo.get.value,
            preComputedValues(lastSegment.endNode()), preComputedValues(nodeAfterTo.get.value), toLeftResidueLength, toRightResidueLength)
        else
          (toImpactedSegment, None)

      var newSegments: QList[Segment[T]] = segmentsAfterToImpactedSegment
      if(rightResidue.nonEmpty) newSegments = QList(rightResidue.get, newSegments)
      if(leftResidue.nonEmpty) newSegments = QList(leftResidue.get, newSegments)
      newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)
      ListSegments(newSegments,vehicle)
    }

    /**
      * This method finds the impacted segment of the previous update.
      * The segment is found if the endNode is after or equal to the search position.
      *
      * @param pos the searched position
      * @param vehicle the vehicle in which we want to add a node
      * @return a tuple (impactedSegment: Segment[T], exploredSegments: Option[QList[Segment[T] ] ], unexploredSegments: Option[QList[Segment[T] ] ])
      */
    private def findImpactedSegment(pos: Long, vehicle: Int, segmentsToExplore: QList[Segment[T]] = segments, initCounter: Option[Int] = None): (Segment[T], QList[Segment[T]], QList[Segment[T]], Int) ={
      val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)

      def checkSegment(segmentsToExplore: QList[Segment[T]], counter: Int = initCounter.getOrElse(vehiclePos-1), exploredSegments: QList[Segment[T]] = null): (Segment[T], QList[Segment[T]], QList[Segment[T]], Int) ={
        require(segmentsToExplore != null, "Shouldn't happen, it means that the desired position is not within this vehicle route")
        val segment = segmentsToExplore.head
        val newCounter = counter + segment.length
        if(newCounter >= pos)
          (segment, if(exploredSegments != null) exploredSegments.reverse else exploredSegments, segmentsToExplore.tail, counter)
        else
          checkSegment(segmentsToExplore.tail, newCounter, QList(segment,exploredSegments))
      }

      checkSegment(segmentsToExplore)
    }

    def length(): Long ={
      segments.map(_.length()).sum
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
  def splitAtNode(beforeSplitNode: Long, splitNode: Long, valueBeforeSplitNode: T, valueAtSplitNode: T, leftLength: Long, rightLength: Long): (Option[Segment[T]],Option[Segment[T]])

  def flip(): Segment[T]

  def length(): Long

  def startNode(): Long

  def endNode(): Long
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
                                                  endNodeValue:T,
                                                  length: Long) extends Segment[T]{
  override def toString: String = {
    "PreComputedSubSequence (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + " Length : " + length + ")"
  }

  override def splitAtNode(beforeSplitNode: Long, splitNode: Long, valueBeforeSplitNode: T, valueAtSplitNode: T, leftLength: Long, rightLength: Long): (Option[Segment[T]],Option[Segment[T]]) = {
    if(splitNode == startNode) (None,Some(this))
    else if(beforeSplitNode == endNode) (Some(this),None)
    else {
      (Some(PreComputedSubSequence(startNode,startNodeValue,beforeSplitNode,valueBeforeSplitNode,leftLength)),
        Some(PreComputedSubSequence(splitNode,valueAtSplitNode,endNode,endNodeValue,rightLength)))
    }
  }

  override def flip(): Segment[T] = {
    FlippedPreComputedSubSequence(endNode, endNodeValue, startNode, startNodeValue, length)
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
                                                         endNodeValue:T,
                                                         length: Long) extends Segment[T]{
  override def toString: String = {
    "FlippedPreComputedSubSequence (StartNode : " + startNode + " - value : " + startNodeValue + " EndNode : " + endNode + " - value " + endNodeValue + ")"
  }

  override def splitAtNode(beforeSplitNode: Long, splitNode: Long, valueBeforeSplitNode: T, valueAtSplitNode: T, leftLength: Long, rightLength: Long): (Option[Segment[T]],Option[Segment[T]]) = {
    if(splitNode == startNode) (None,Some(this))
    else if(beforeSplitNode == endNode) (Some(this),None)
    else {
      (Some(FlippedPreComputedSubSequence(startNode,startNodeValue,beforeSplitNode,valueBeforeSplitNode,leftLength)),
        Some(FlippedPreComputedSubSequence(splitNode,valueAtSplitNode,endNode,endNodeValue,rightLength)))
    }
  }

  override def flip(): Segment[T] = {
    PreComputedSubSequence(endNode, endNodeValue, startNode, startNodeValue, length)
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

  override def splitAtNode(beforeSplitNode: Long, splitNode: Long, valueBeforeSplitNode: T, valueAtSplitNode: T, leftLength: Long, rightLength: Long): (Option[Segment[T]],Option[Segment[T]]) = {
    require(beforeSplitNode == node)
    (Some(this), None)
  }

  override def flip(): Segment[T] = {
    this
  }

  override def length(): Long = 1L

  override def startNode(): Long = node

  override def endNode(): Long = node
}
