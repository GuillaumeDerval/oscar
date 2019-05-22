package oscar.cbls.business.routing.invariants.group

import oscar.cbls._
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core._

abstract class GlobalConstraintDefinitionV2 [T : Manifest, U:Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n = routes.maxValue+1L
  val vehicles = 0L until v

  val preComputedValues: Array[T] = new Array[T](n)
  val vehicleValues : Array[U] = new Array[U](v)
  var vehicleValuesAtLevel0 : Array[U] = new Array[U](v)
  var checkpointAtLevel0: IntSequence = _
  var changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, true)
  var checkpointLevel: Int = -1
  var savedDataAtCheckPointLevel: QList[(Int,Option[Array[ListSegments]], VehicleLocation, IterableMagicBoolArray, Array[Option[Int]])] = null
  // TODO : if at checkpoint 1 only 1 vehicle has changed, all the positions before that vehicle are still valid an thus don't need to be recomputed
  //        It should be useful when exploring wildly
  var positionToValueCache: Array[Option[Int]] = Array.fill(n)(None)

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
          changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(vehicle => {
            performPreCompute(vehicle,newRoute,preComputedValues)
            vehicleValuesAtLevel0(vehicle) = vehicleValues(vehicle)
          })
          changedVehiclesSinceCheckpoint0.all_=(false)
          vehicleSearcher = vehicleSearcher.regularize
          savedDataAtCheckPointLevel = null
        }

        // If false it means that we had an assign value update => not incremental => from scratch
        val segmentsAtCheckPoint = if (prevUpdateSegments.isEmpty || this.checkpointLevel < 0) {
          computeAndAssignVehiclesValueFromScratch(changes.newValue)
          Some(Array.tabulate(v)(vehicle => initSegmentsOfVehicle(vehicle,newRoute)))
        } else {
          prevUpdateSegments
        }

        this.checkpointLevel = checkpointLevel
        if(checkpointLevel > 0) {
          val previousCheckpointSaveData = savedDataAtCheckPointLevel.head
          savedDataAtCheckPointLevel = QList((previousCheckpointSaveData._1, previousCheckpointSaveData._2, previousCheckpointSaveData._3, previousCheckpointSaveData._4,
            positionToValueCache),savedDataAtCheckPointLevel.tail)
        }
        positionToValueCache = Array.fill(n)(None)
        savedDataAtCheckPointLevel = QList((checkpointLevel,segmentsAtCheckPoint,vehicleSearcher,changedVehiclesSinceCheckpoint0,positionToValueCache),savedDataAtCheckPointLevel)

        segmentsAtCheckPoint



      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0L) {
          require(checkpoint quickEquals this.checkpointAtLevel0)
          for(vehicle <- 0 until v) vehicleValues(vehicle) = vehicleValuesAtLevel0(vehicle)
        }
        while(savedDataAtCheckPointLevel.tail != null  && savedDataAtCheckPointLevel.tail.head._1 >= checkpointLevel)
          savedDataAtCheckPointLevel = savedDataAtCheckPointLevel.tail
        val (_, segments, vehicleSearcherAtCheckpointLevel, changedVehiclesSinceCheckpoint, positionToValueCacheAtCheckpoint) = savedDataAtCheckPointLevel.head
        vehicleSearcher = vehicleSearcherAtCheckpointLevel
        changedVehiclesSinceCheckpoint0 = changedVehiclesSinceCheckpoint
        positionToValueCache = positionToValueCacheAtCheckpoint
        segments

      case sui@SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          keepOrResetPositionValueCache(prev)

          val prevRoutes = prev.newValue
          // TODO : Probably more interesting to put those value in a local var and update/save his value using the checkpoint system
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
          keepOrResetPositionValueCache(prev)

          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get
          val newPrevUpdateSegments = Array.tabulate(v)(lastPrevUpdateSegments)

          val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
          val toVehicle = vehicleSearcher.vehicleReachingPosition(after)

          // Identification of the sub-segments to remove
          val (listSegmentAfterRemove, segmentsToRemove) =
            newPrevUpdateSegments(fromVehicle).removeSubSegments(fromIncluded, toIncluded, prevRoutes)

          newPrevUpdateSegments(fromVehicle) = listSegmentAfterRemove

          // If we are in same vehicle and we remove nodes to put them later in the route, the route length before insertion point has shortened
          val delta =
            if(fromVehicle != toVehicle || after < fromIncluded) 0
            else toIncluded - fromIncluded + 1

          // Insert the sub-segments at his new position
          val segmentsAfterInsertion =
            if(flip)
              newPrevUpdateSegments(toVehicle).insertSegments(segmentsToRemove.qMap(_.flip).reverse, after, prevRoutes, delta)
            else
              newPrevUpdateSegments(toVehicle).insertSegments(segmentsToRemove, after, prevRoutes, delta)

          newPrevUpdateSegments(toVehicle) = segmentsAfterInsertion

          vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)

          changedVehiclesSinceCheckpoint0(fromVehicle) = true
          changedVehiclesSinceCheckpoint0(toVehicle) = true

          Some(newPrevUpdateSegments)
        }

      case sur@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          keepOrResetPositionValueCache(prev)

          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get
          val newPrevUpdateSegments = Array.tabulate(v)(lastPrevUpdateSegments)

          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)

          val (listSegmentAfterRemove, _) = newPrevUpdateSegments(impactedVehicle).removeSubSegments(position,position,prevRoutes)

          newPrevUpdateSegments(impactedVehicle) = listSegmentAfterRemove

          vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true

          Some(newPrevUpdateSegments)
        }

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        val vehiclesListSegments: Array[ListSegments] = if(savedDataAtCheckPointLevel == null) Array.fill(v)(null) else savedDataAtCheckPointLevel.head._2.get
        (0 until v).foreach(vehicle => vehiclesListSegments(vehicle) = initSegmentsOfVehicle(vehicle, value))
        Some(vehiclesListSegments)

      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  private def keepOrResetPositionValueCache(prev: SeqUpdate): Unit ={
    prev match {
      case _:SeqUpdateInsert => positionToValueCache = Array.fill(n)(None)
      case _:SeqUpdateMove => positionToValueCache = Array.fill(n)(None)
      case _:SeqUpdateRemove => positionToValueCache = Array.fill(n)(None)
      case _ =>
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
    def removeSubSegments(from: Int, to: Int, routes: IntSequence): (ListSegments, QList[Segment[T]]) ={
      val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)
      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment, currentCounter) =
        findImpactedSegment(from,vehicle,vehiclePos-1)
      val (toImpactedSegment, segmentsBetweenFromAndTo, segmentsAfterToImpactedSegment,_) =
        findImpactedSegment(to,vehicle, currentCounter,QList(fromImpactedSegment,segmentsAfterFromImpactedSegment))

      // nodeBeforeFrom is always defined because it's at worst a vehicle node
      val (nodeBeforeFrom, fromNode, toNode, nodeAfterTo) = {
        val beforeFromExplorer = if(positionToValueCache(from-1).isEmpty)routes.explorerAtPosition(from-1) else None
        val fromExplorer = if(positionToValueCache(from).isEmpty){
          if(beforeFromExplorer.nonEmpty)
            beforeFromExplorer.get.next
          else
            routes.explorerAtPosition(from)
        } else None

        val toExplorer = if(positionToValueCache(to).isEmpty) routes.explorerAtPosition(to) else None

        if(beforeFromExplorer.nonEmpty)
          positionToValueCache(from-1) = Some(beforeFromExplorer.get.value)
        if(fromExplorer.nonEmpty)
          positionToValueCache(from) = Some(fromExplorer.get.value)
        if(toExplorer.nonEmpty)
          positionToValueCache(to) = Some(toExplorer.get.value)
        if(to+1 < n && positionToValueCache(to+1).isEmpty){
          val afterToExplorer =
            if(toExplorer.nonEmpty)
              toExplorer.get.next
            else
              routes.explorerAtPosition(to+1)
          positionToValueCache(to+1) = Some(if(afterToExplorer.isEmpty) -1 else afterToExplorer.get.value)
        }
        (positionToValueCache(from-1).get,
          positionToValueCache(from).get,
          positionToValueCache(to).get,
          if(to+1 < n)positionToValueCache(to+1).get else -1)
      }

      val lengthUntilFromImpactedSegment = QList.qFold[Segment[T],Long](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0)

      // 1° The removed segment is included in one segment
      val (leftResidue, removedSegments, rightResidue) =
        if(fromImpactedSegment == toImpactedSegment) {
          val fromLeftResidueLength = from - lengthUntilFromImpactedSegment - vehiclePos
          val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength

          val toRightResidueLength =
            if (nodeAfterTo == -1) 0
            else fromRightResidueLength - to + from - 1
          val toLeftResidueLength =
            fromRightResidueLength - toRightResidueLength

          // The left-remaining part of the initial segment
          val (leftResidue, leftCuttedInitialSegment: Option[Segment[T]]) =
            fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode), fromLeftResidueLength, fromRightResidueLength)
          // The right-remaining part of the initial segment
          val (removedSegments: Option[Segment[T]], rightResidue) =
            if (nodeAfterTo >= 0)
              leftCuttedInitialSegment.get.splitAtNode(toNode, nodeAfterTo, preComputedValues(toNode), preComputedValues(nodeAfterTo), toLeftResidueLength, toRightResidueLength)
            else
              (leftCuttedInitialSegment, None)

          (leftResidue, QList(removedSegments.get), rightResidue)
        } else {
          val lengthToFromImpactedSegment =
            lengthUntilFromImpactedSegment +
              fromImpactedSegment.length()
          val lengthToToImpactedSegment =
            lengthUntilFromImpactedSegment +
              QList.qFold[Segment[T],Long](segmentsBetweenFromAndTo, (acc,item) => acc + item.length(),0) +
              toImpactedSegment.length()

          val toRightResidueLength =
            if (nodeAfterTo == -1) 0
            else lengthToToImpactedSegment - to + vehiclePos
          val toLeftResidueLength =
            lengthToToImpactedSegment - toRightResidueLength

          val fromRightResidueLength = lengthToFromImpactedSegment - from + vehiclePos
          val fromLeftResidueLength = lengthToFromImpactedSegment - fromRightResidueLength

          // The left-remaining part of the initial from segment
          val (leftResidue, leftCuttedFromSegment: Option[Segment[T]]) =
            fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, preComputedValues(nodeBeforeFrom), preComputedValues(fromNode), fromLeftResidueLength, fromRightResidueLength)
          // The right-remaining part of the initial to segment
          val (rightCuttedToSegment: Option[Segment[T]], rightResidue) =
            if (nodeAfterTo >= 0)
              toImpactedSegment.splitAtNode(toNode, nodeAfterTo, preComputedValues(toNode), preComputedValues(nodeAfterTo), toLeftResidueLength, toRightResidueLength)
            else
              (toImpactedSegment, None)

          var removedSegments =
            QList(leftCuttedFromSegment.get, segmentsBetweenFromAndTo.tail)
          if (rightCuttedToSegment.nonEmpty)
            removedSegments = QList.nonReversedAppend(removedSegments, QList(rightCuttedToSegment.get))

          (leftResidue, removedSegments, rightResidue)
        }

      var newSegments: QList[Segment[T]] = segmentsAfterToImpactedSegment
      if(rightResidue.nonEmpty) newSegments = QList(rightResidue.get, newSegments)
      if(leftResidue.nonEmpty) newSegments = QList(leftResidue.get, newSegments)
      newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)

      (ListSegments(newSegments,vehicle),removedSegments)
    }

    /**
      * Insert a list of segments at the specified position
      * @param segmentsToInsert
      * @param afterPosition
      * @return
      */
    def insertSegments(segmentsToInsert: QList[Segment[T]], afterPosition: Int, routes: IntSequence, delta: Long = 0): ListSegments ={
      val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)

      val (impactedSegment, segmentsBeforeImpactedSegment, segmentsAfterImpactedSegment,_) = findImpactedSegment(afterPosition - delta, vehicle, vehiclePos-1)

      val (insertAfterNode, insertBeforeNode) = {
        val afterExplorer = if(positionToValueCache(afterPosition).isEmpty) routes.explorerAtPosition(afterPosition) else None

        if(afterExplorer.nonEmpty)
          positionToValueCache(afterPosition) = Some(afterExplorer.get.value)
        if(afterPosition+1 < n && positionToValueCache(afterPosition+1).isEmpty){
          val beforeExplorer = if(afterExplorer.nonEmpty)
              afterExplorer.get.next
            else
              routes.explorerAtPosition(afterPosition+1)
          positionToValueCache(afterPosition+1) = Some(if(beforeExplorer.isEmpty)-1 else beforeExplorer.get.value)
        }
        (positionToValueCache(afterPosition).get, if(afterPosition+1 < n)positionToValueCache(afterPosition+1).get else -1)
      }

      val rightResidueLength =
        if(insertBeforeNode == -1) 0
        else
          QList.qFold[Segment[T],Long](segmentsBeforeImpactedSegment, (acc,item) => acc + item.length(),0) +
            impactedSegment.length() - (afterPosition + 1) + vehiclePos
      val leftResidueLength = impactedSegment.length() - rightResidueLength

      // We need to split the impacted segment in two and insert the new Segment between those two sub segment
      // If we are at the end of a vehicle route, we don't split the route
      val (leftResidue: Option[Segment[T]], rightResidue: Option[Segment[T]]) =
        if(insertBeforeNode >= v)
          impactedSegment.splitAtNode(insertAfterNode,insertBeforeNode,
            preComputedValues(insertAfterNode),preComputedValues(insertBeforeNode),leftResidueLength, rightResidueLength)
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
      * This method finds the impacted segment of the previous update.
      * The segment is found if the endNode is after or equal to the search position.
      *
      * @param pos the searched position
      * @param vehicle the vehicle in which we want to add a node
      * @return a tuple (impactedSegment: Segment[T], exploredSegments: Option[QList[Segment[T] ] ], unexploredSegments: Option[QList[Segment[T] ] ])
      */
    private def findImpactedSegment(pos: Long, vehicle: Int, initCounter: Long, segmentsToExplore: QList[Segment[T]] = segments): (Segment[T], QList[Segment[T]], QList[Segment[T]], Int) ={
      def checkSegment(segmentsToExplore: QList[Segment[T]], counter: Int = initCounter, exploredSegments: QList[Segment[T]] = null): (Segment[T], QList[Segment[T]], QList[Segment[T]], Int) ={
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
      QList.qFold[Segment[T], Long](segments, (acc, item) => acc + item.length, 0L)
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
