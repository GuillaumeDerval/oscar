package oscar.cbls.business.routing.invariants.group

import oscar.cbls._
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.model.{RoutingConventionMethods, VehicleLocation}
import oscar.cbls.core._

import scala.collection.immutable.{HashMap, Stack}
import scala.collection.mutable
/*
class GlobalConstraintDefinitionV2 [T : Manifest, U:Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n = routes.maxValue+1L
  val vehicles = 0L until v

  val preComputedValues: Array[T] = new Array[T](n)
  val vehicleValues : Array[U] = new Array[U](v)
  var checkpointAtLevel0: IntSequence = _
  val changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, true)
  var changedVehiclesSinceLastApplyUpdate = new IterableMagicBoolArray(v,false)

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

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

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    digestUpdates(changes) match{
      case None => for (vehicle <- vehicles){
        assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
      }
      case Some(x) if checkpointLevel =>
        for (vehicle <- vehicles){
          assignVehicleValue(vehicle,computeVehicleValueFromScratch(vehicle,routes.value))
        }
      case Some(x) => x.last.foreach(vehicleSegments =>
        if(vehicleSegments.segments.length > 1)
          computeVehicleValue(vehicleSegments.vehicle,vehicleSegments.segments,changes.newValue,preComputedValues)
    }
  }

  private def digestUpdates(changes:SeqUpdate): Option[List[Array[ListSegments]]] = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        val newRoute = changes.newValue

        val prevUpdateSegments = digestUpdates(prev)
        // If false it means that we had an assign value update => not incremental => from scratch
        if (prevUpdateSegments.isEmpty) {
          computeAndAssignVehiclesValueFromScratch(changes.newValue)
        }

        changedVehiclesSinceCheckpoint0.indicesAtTrue.foreach(performPreCompute(_, newRoute, preComputedValues))
        prevUpdateSegments

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0L) {
          require(checkpoint quickEquals this.checkpoint)
          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }

      case sui@SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get.head
          val newPrevUpdateSegments = Array.tabulate(n)(lastPrevUpdateSegments)

          vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos)

          newPrevUpdateSegments(impactedVehicle) = newPrevUpdateSegments(impactedVehicle).insertSegments(List(NewNode(value)),pos,prevRoutes)
          Some(newPrevUpdateSegments :: prevUpdateSegments.get)
        }


      case sum@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get.head
          val newPrevUpdateSegments = Array.tabulate(n)(lastPrevUpdateSegments)

          vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)
          val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
          val toVehicle = vehicleSearcher.vehicleReachingPosition(toVehicle)

          val (newFromVehicleListSegment, removedSegments) = newPrevUpdateSegments(fromVehicle).removeSegments(fromIncluded, toIncluded, prevRoutes)
          newPrevUpdateSegments(fromVehicle) = newFromVehicleListSegment
          if(flip)
            newPrevUpdateSegments(toVehicle) = newPrevUpdateSegments(toVehicle).insertSegments(removedSegments.map(_.flip), after, prevRoutes)
          else
            newPrevUpdateSegments(toVehicle) = newPrevUpdateSegments(toVehicle).insertSegments(removedSegments, after, prevRoutes)
          Some(newPrevUpdateSegments :: prevUpdateSegments.get)
        }

      case sur@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val prevUpdateSegments = digestUpdates(prev)
        if(prevUpdateSegments.isEmpty) None
        else {
          val prevRoutes = prev.newValue
          val lastPrevUpdateSegments = prevUpdateSegments.get.head
          val newPrevUpdateSegments = Array.tabulate(n)(lastPrevUpdateSegments)

          val vehicle = vehicleSearcher.vehicleReachingPosition(position)
          newPrevUpdateSegments(vehicle) = newPrevUpdateSegments(vehicle).removeSegments(position,position,prevRoutes)._1
          vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
          Some(newPrevUpdateSegments :: prevUpdateSegments.get)
        }

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        changedVehiclesSinceLastApplyUpdate.all_=(false)
        Some(List(Array.tabulate(v)(vehicle => {
          val lastNodeOfVehicle =
            if(vehicle < v-1) value.valueAtPosition(vehicleSearcher.startPosOfVehicle(vehicle+1)-1).get
            else value.valueAtPosition(value.size-1).get
          ListSegments(List(PreComputedSubSequence(vehicle, preComputedValues(vehicle), lastNodeOfVehicle, preComputedValues(lastNodeOfVehicle))),vehicle)
        })))
      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  private def computeAndAssignVehiclesValueFromScratch(newSeq: IntSequence): Unit ={
    vehicles.foreach(v => assignVehicleValue(v, computeVehicleValueFromScratch(v, newSeq)))
  }



  case class ListSegments(segments: List[Segment[T]], vehicle: Int){
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
    def removeSegments(from: Int, to: Int, routes: IntSequence): (ListSegments, List[Segment[T]]) ={
      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment) = findImpactedSegment(from,routes)
      val (toImpactedSegment, segmentsBeforeToImpactedSegment) = findImpactedSegment(to,routes,segments.drop(segmentsBeforeFromImpactedSegment.length))
      // nodeBeforeFrom is always defined because it's at worst a vehicle node
      val nodeBeforeFrom = routes.explorerAtPosition(from - 1)
      val nodeAfterTo = routes.explorerAtPosition(to + 1)

      val (x1,x2) = fromImpactedSegment.splitAtNode(nodeBeforeFrom.get.value,from,preComputedValues(nodeBeforeFrom.get.value),preComputedValues(from))
      if(fromImpactedSegment == toImpactedSegment)
        if(nodeAfterTo.isEmpty) {
          val newListSegments = ListSegments(segmentsBeforeFromImpactedSegment ::: x1.get :: Nil, vehicle)
          val removedSegments = List(x2.get)
          (newListSegments,removedSegments)
        } else {
          val (y1, y2) = x2.get.splitAtNode(to, nodeAfterTo.get.value, preComputedValues(to), preComputedValues(nodeAfterTo.get.value))
          val segmentsAfterToImpactedSegment =
            segments.drop(
              segmentsBeforeFromImpactedSegment.length +    // Segments before the removed one
              1                                             // The impacted segment (same for from and to)
            )
          val newListSegments = ListSegments(
            segmentsBeforeFromImpactedSegment ::: x1.get :: y2.get :: segmentsAfterToImpactedSegment,
            vehicle)
          val removedSegments = List(y1.get)
          (newListSegments,removedSegments)
        }
      else{
        if(nodeAfterTo.isEmpty) {
          val newListSegments = ListSegments(segmentsBeforeFromImpactedSegment ::: x1.get :: Nil, vehicle)
          val removedSegments = x2.get :: segmentsBeforeToImpactedSegment.drop(1) ::: List(toImpactedSegment)
          (newListSegments,removedSegments)
        } else {
          val (y1, y2) = toImpactedSegment.splitAtNode(to, nodeAfterTo.get.value, preComputedValues(to), preComputedValues(nodeAfterTo.get.value))
          val segmentsAfterToImpactedSegment =
            segments.drop(
              segmentsBeforeFromImpactedSegment.length +    // Segments before the removed one
                2 +                                         // The two impacted segments
                segmentsBeforeToImpactedSegment.length - 1  // Segments between the two impacted segments (which includes fromImpactedSegment)
            )
          val newListSegments = ListSegments(
            segmentsBeforeFromImpactedSegment ::: x1.get :: y2.get :: segmentsAfterToImpactedSegment,
            vehicle)
          val removedSegments = x2.get :: segmentsBeforeToImpactedSegment.drop(1) ::: List(y1.get)
          (newListSegments,removedSegments)
        }

      }
    }

    /**
      * Insert a list of segments after the specified position
      * @param segmentsToInsert
      * @param afterPosition
      * @return
      */
    def insertSegments(segmentsToInsert: List[Segment[T]], afterPosition: Int, routes: IntSequence): ListSegments ={
      val (impactedSegment, _) = findImpactedSegment(afterPosition, routes)
      // Browsing the segments list until we find the impactedSegment
      val newSegments = segments.foldLeft(List[Segment[T]]())(
        (acc, currentSegment) => {
          if (currentSegment == impactedSegment) {
            // We need to split the impacted segment in two and insert the new Segment between those two sub segment
            val insertAfterNode = routes.explorerAtPosition(afterPosition).get.value
            val insertBeforeNode = routes.explorerAtPosition(afterPosition + 1).get.value
            val (segmentBeforeInsertion, segmentAfterInsertion) =
              currentSegment.splitAtNode(insertBeforeNode, insertAfterNode, preComputedValues(insertBeforeNode), preComputedValues(insertAfterNode))

            if(segmentAfterInsertion.nonEmpty)
              segmentAfterInsertion.get :: segmentsToInsert ::: segmentBeforeInsertion.get :: acc
            else
              segmentsToInsert ::: segmentBeforeInsertion.get :: acc
          } else {
            currentSegment :: acc
          }
        }).reverse
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
      * @return a tuple (impactedSegment: Segment[T], exploredSegments: List[Segment[T] ])
      */
    private def findImpactedSegment(pos: Int, routes: IntSequence,
                                    currentSegments: List[Segment[T]] = segments,
                                    exploredSegments: List[Segment[T]] = List.empty): (Segment[T], List[Segment[T]]) ={
      require(currentSegments.nonEmpty, "Segments list shouldn't be empty, if this append it means that the searched position isn't reach by this vehicle's segments")
      if(positionWithinSegment(pos, routes, currentSegments.head)) (currentSegments.head,exploredSegments.reverse)
      else findImpactedSegment(pos,routes,currentSegments.tail,currentSegments.head :: exploredSegments)
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
  }

}*/

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
    require(splitNode == node)
    (None,Some(this))
  }

  override def flip(): Segment[T] = {
    this
  }
}
