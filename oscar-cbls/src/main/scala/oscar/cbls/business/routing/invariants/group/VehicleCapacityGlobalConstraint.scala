package oscar.cbls.lib.invariant.routing.capa.stagequentinmeurisse
/*
import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group.PreComputeInvariant
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}
import oscar.cbls.core.propagation.Checker

*/
/**
  * @author Quentin Meurisse
  *
  *

object VehicleCapacityGlobalConstraint {
  def apply(routes: ChangingSeqValue,
            v: Long,
            deltaAtNode: Array[Long],
            maxCapacity: Long,
            violation: Array[CBLSIntVar],
            contentAtEndOfVehicleRoute: Array[CBLSIntVar]): VehicleCapacityGlobalConstraint =
    new VehicleCapacityGlobalConstraint(routes, v, deltaAtNode, maxCapacity, violation, contentAtEndOfVehicleRoute)
}

class VehicleCapacityGlobalConstraint(routes: ChangingSeqValue,
                                      v: Long,
                                      deltaAtNode: Array[Long],
                                      maxCapacity: Long,
                                      violation: Array[CBLSIntVar],
                                      contentAtEndOfVehicleRoute: Array[CBLSIntVar])
  extends PreComputeInvariant[PreComputeClass, SavedValuesAtCheckpoint](routes, v) {

  registerStaticAndDynamicDependency(routes)
  this.finishInitialization()
  violation.foreach(_.setDefiningInvariant(this))
  contentAtEndOfVehicleRoute.foreach(_.setDefiningInvariant(this))


  def addToReachCount(level: Long, addTo: RedBlackTreeMap[Long]): RedBlackTreeMap[Long] = {
    addTo.insert(level, addTo.getOrElse(level, 0L) + 1L)
  }

  /**
    * computes the integral on x of(toFunction(x) - fromFunction(x)) with x in [minValueIncluded, maxValueIncluded]
    *
    * @param fromFunction
    * @param toFunction
    * @param minValueIncluded
    * @param maxValueIncluded
    * @return
    */
  def computeIntegralInBoundsAbove(fromFunction: RedBlackTreeMap[Long], toFunction: RedBlackTreeMap[Long], minValueIncluded: Long, maxValueIncluded: Long): Long = {
    //the integral goes from high to low values

    @inline
    def stepIntegral(nextPositionOnFromOpt: Option[RedBlackTreeMapExplorer[Long]], nextPositionOnToOpt: Option[RedBlackTreeMapExplorer[Long]],
                     positionOfIntegrator: Long, width: Long, acc: Long,
                     deltaWidth: Long, pivotValue: Long): Long = {
      if (pivotValue > maxValueIncluded) {
        //this pivot is still above the integration box
        require(acc == 0L)
        computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt, nextPositionOnToOpt, pivotValue, width + deltaWidth, acc)
      } else {
        //this pivot is below or equal to the maxValueIncluded
        val valueAboveIncluded = if (positionOfIntegrator > maxValueIncluded) maxValueIncluded else positionOfIntegrator
        if (pivotValue >= minValueIncluded) {
          //just a square added, and carry on
          computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt, nextPositionOnToOpt, pivotValue, width + deltaWidth,
            acc + (valueAboveIncluded - pivotValue) * width)
        } else {
          //add a square and finish
          acc + (valueAboveIncluded - minValueIncluded + 1L) * width
        }
      }
    }


    def computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt: Option[RedBlackTreeMapExplorer[Long]],
                                            nextPositionOnToOpt: Option[RedBlackTreeMapExplorer[Long]],
                                            positionOfIntegrator: Long, width: Long, acc: Long): Long = {
      (nextPositionOnFromOpt, nextPositionOnToOpt) match {
        case (None, None) =>
          if (positionOfIntegrator >= maxValueIncluded) {
            acc + width * (maxValueIncluded - minValueIncluded + 1L)
          } else if (positionOfIntegrator >= minValueIncluded) {
            acc + width * (positionOfIntegrator - minValueIncluded + 1L)
          } else {
            acc
          }
        case (Some(nextPositionOnFrom), None) =>
          stepIntegral(
            nextPositionOnFrom.prev, nextPositionOnToOpt, positionOfIntegrator, width, acc,
            -nextPositionOnFrom.value, nextPositionOnFrom.key)
        case (None, Some(nextPositionOnTo)) =>
          stepIntegral(
            nextPositionOnFromOpt, nextPositionOnTo.prev, positionOfIntegrator, width, acc,
            nextPositionOnTo.value, nextPositionOnTo.key)
        case (Some(nextPositionOnFrom), Some(nextPositionOnTo)) =>
          if (nextPositionOnFrom.key == nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.prev, nextPositionOnTo.prev, positionOfIntegrator, width, acc,
              nextPositionOnTo.value - nextPositionOnFrom.value,
              nextPositionOnTo.key)
          else if (nextPositionOnFrom.key < nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFromOpt, nextPositionOnTo.prev, positionOfIntegrator, width, acc,
              nextPositionOnTo.value, nextPositionOnTo.key)
          else
            stepIntegral(
              nextPositionOnFrom.prev, nextPositionOnToOpt, positionOfIntegrator, width, acc,
              -nextPositionOnFrom.value, nextPositionOnFrom.key)
      }
    }

    computeIntegralInBoundsOptFromOptTo(fromFunction.biggestPosition, toFunction.biggestPosition, Long.MaxValue, 0L, 0L)
  }

  /**
    * compute the area between maxValueIncluded + 1L and (toFunction(x) - fromFunction(x)) for x in [minValueIncluded, maxValueIncluded]
    *
    * @param fromFunction
    * @param toFunction
    * @param minValueIncluded
    * @param maxValueIncluded
    * @return
    */

  def computeIntegralInBoundsUnder(fromFunction: RedBlackTreeMap[Long], toFunction: RedBlackTreeMap[Long], minValueIncluded: Long, maxValueIncluded: Long): Long = {
    // the integral goes from low to high values
    @inline
    def stepIntegral(nextPositionOnFromOpt: Option[RedBlackTreeMapExplorer[Long]], nextPositionOnToOpt: Option[RedBlackTreeMapExplorer[Long]],
                     positionOfIntegrator: Long, width: Long, acc: Long,
                     deltaWidth: Long, pivotValue: Long): Long = {
      if (pivotValue < minValueIncluded) {
        require(acc == 0L)
        computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt, nextPositionOnToOpt, pivotValue, width + deltaWidth, acc)
      } else {
        val valueUnderIncluded = if (positionOfIntegrator < minValueIncluded) minValueIncluded else positionOfIntegrator
        if (pivotValue <= maxValueIncluded) {
          computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt, nextPositionOnToOpt, pivotValue, width + deltaWidth,
            acc + (pivotValue - valueUnderIncluded) * width)
        } else {
          acc + (maxValueIncluded - valueUnderIncluded + 1L) * width
        }
      }
    }

    def computeIntegralInBoundsOptFromOptTo(nextPositionOnFromOpt: Option[RedBlackTreeMapExplorer[Long]],
                                            nextPositionOnToOpt: Option[RedBlackTreeMapExplorer[Long]],
                                            positionOfIntegrator: Long, width: Long, acc: Long): Long = {
      (nextPositionOnFromOpt, nextPositionOnToOpt) match {
        case (None, None) =>
          if (positionOfIntegrator <= minValueIncluded) {
            acc + width * (maxValueIncluded - minValueIncluded + 1L)
          } else if (positionOfIntegrator <= maxValueIncluded) {
            acc + width * (maxValueIncluded - positionOfIntegrator + 1L)
          } else {
            acc
          }
        case (Some(nextPositionOnFrom), None) =>
          stepIntegral(
            nextPositionOnFrom.next, nextPositionOnToOpt, positionOfIntegrator, width, acc,
            -nextPositionOnFrom.value, nextPositionOnFrom.key)
        case (None, Some(nextPositionOnTo)) =>
          stepIntegral(
            nextPositionOnFromOpt, nextPositionOnTo.next, positionOfIntegrator, width, acc,
            nextPositionOnTo.value, nextPositionOnTo.key)
        case (Some(nextPositionOnFrom), Some(nextPositionOnTo)) =>
          if (nextPositionOnFrom.key == nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.next, nextPositionOnTo.next, positionOfIntegrator, width, acc,
              nextPositionOnTo.value - nextPositionOnFrom.value,
              nextPositionOnTo.key)
          else if (nextPositionOnFrom.key < nextPositionOnTo.key)
            stepIntegral(
              nextPositionOnFrom.next, nextPositionOnToOpt, positionOfIntegrator, width, acc,
              -nextPositionOnFrom.value, nextPositionOnFrom.key)
          else
            stepIntegral(
              nextPositionOnFromOpt, nextPositionOnTo.next, positionOfIntegrator, width, acc,
              nextPositionOnTo.value, nextPositionOnTo.key)
      }
    }

    computeIntegralInBoundsOptFromOptTo(fromFunction.smallestPosition, toFunction.smallestPosition, Long.MinValue, 0L, 0L)
  }

  /**
    *
    * @param seq
    * @param vehicle
    * @return (the violation of the vehicle; the content of the vehicle when returning to its home)
    */
  def computeViolationFromScratchNoPrecompute(seq: IntSequence, vehicle: Long): (Long, Long) = {
    var currentContent = deltaAtNode(vehicle)
    var viol = if (currentContent > maxCapacity) currentContent - maxCapacity // violation at start node
    else 0L
    var explorerOpt = seq.explorerAtAnyOccurrence(vehicle).head.next

    while (explorerOpt match {
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if (node >= v) {
          //continuing the same vehicle
          currentContent = currentContent + deltaAtNode(node)
          if (currentContent > maxCapacity) {
            //overshoot, add to violation, 1L hop
            viol += (currentContent - maxCapacity)
          }
          explorerOpt = explorer.next
          true
        } else {
          //at the next vehicle
          false
        }
    }) {}
    (viol, currentContent)
  }

  override def neutralElement: PreComputeClass = SumOfPreCompute(0L, 0L)

  def isNeutralElement(x: PreComputeClass): Boolean = {
    x match {
      case SumOfPreCompute(0L, 0L) => true
      case _ => false
    }
  }

  /**
    *
    * @param x an instance of PreComputeContainer or the neutral element
    * @param y an instance of PreComputeContainer
    * @param reverse is the segment flipped or not
    * @return an instance of DeltaOfPreCompute
    */
  override def minus(x: PreComputeClass, y: PreComputeClass, reverse: Boolean): PreComputeClass = {

    val a =
      if (isNeutralElement(x))
        PreComputeContainer(None, None, 0L)
      else
        x.asInstanceOf[PreComputeContainer]
    val b = y.asInstanceOf[PreComputeContainer]

    if (!reverse) {

      val deltaOfContent = b.contentAtNodeAtCheckpoint - a.contentAtNodeAtCheckpoint
      DeltaOfPreComputeForSegment(a.rb, b.rb, deltaOfContent, a.contentAtNodeAtCheckpoint)
    }
    else {
      val deltaOfContent = b.contentAtNodeAtCheckpoint - a.contentAtNodeAtCheckpoint

      val fromNode = a.prevNodeAtCheckpoint0
      val toNode = b.prevNodeAtCheckpoint0.get
      val integralFrom = fromNode match {
        case None => RedBlackTreeMap.empty[Long]
        case Some(node) =>
          a.node match {
            case None => RedBlackTreeMap.empty[Long]
            case Some(n) if n < v => RedBlackTreeMap.empty[Long]
            case Some(n) if n >= v => preComputedValues(n).asInstanceOf[PreComputeContainer].rb
          }
      }
      val integralTo = preComputedValues(toNode).asInstanceOf[PreComputeContainer].rb

      DeltaOfPreComputeForFlippedSegment(integralFrom, integralTo, deltaOfContent, b.contentAtNodeAtCheckpoint)
    }
  }

  /**
    *
    * @param x an instance of SumOfPreCompute
    * @param y an instance of DeltaOfPreCompute
    * @return thanks the information contains in x and y we can compute the integral on the segment which defined y.
    *         The result of this integral is added to the cumulative of sum the violations contained in x
    */
  override def plus(x: PreComputeClass, y: PreComputeClass): PreComputeClass = {
    val a = x.asInstanceOf[SumOfPreCompute]
    val b = y.asInstanceOf[DeltaOfPreCompute]
    val toReturn = b match {

      case DeltaOfPreComputeForSegment(fromRB, toRB, deltaOfContent, contentAtPrevNode) =>
        val diffOfContentOnSegmentSinceCheckpoint = a.totalContent - contentAtPrevNode
        val violationOnSegment =
          computeIntegralInBoundsAbove(
            fromRB,
            toRB,
            maxCapacity - diffOfContentOnSegmentSinceCheckpoint + 1L,
            toRB.biggestPosition.get.key)

        SumOfPreCompute(a.totalViolation + violationOnSegment, a.totalContent + deltaOfContent)

      case DeltaOfPreComputeForFlippedSegment(fromRB, toRB, deltaOfContent, contentAtPrevNode) =>

        val violationOnSegment =
          computeIntegralInBoundsUnder(
            fromRB,
            toRB,
            toRB.smallestPosition.get.key,
            a.totalContent + contentAtPrevNode - maxCapacity - 1L)

        SumOfPreCompute(a.totalViolation + violationOnSegment, a.totalContent + deltaOfContent)

      case DeltaFromScratch(fromNode, toNode) =>
        var content = a.totalContent
        var viol = a.totalViolation
        var explorer = routes.newValue.explorerAtAnyOccurrence(fromNode).get
        while ( {
          val currentNode = explorer.value
          content += deltaAtNode(currentNode)
          if (content - maxCapacity > 0L)
            viol += (content - maxCapacity)
          if (currentNode == toNode) false
          else {
            explorer = explorer.next.get
            true
          }
        }) {}
        SumOfPreCompute(viol, content)
    }
    toReturn
  }

  override def nodesToPreCompute(fromNode: Long, toNode: Long): PreComputeClass = DeltaFromScratch(fromNode, toNode)


  override def computeAndAffectOutputFromScratch(seq: IntSequence) = {
    for (vehicle <- vehicles) {
      recordTouchedVehicleSinceCheckpoint0(vehicle)
      val (viol, contentAtEnd) = computeViolationFromScratchNoPrecompute(seq, vehicle)
      violation(vehicle) := viol
      contentAtEndOfVehicleRoute(vehicle) := contentAtEnd
    }
  }

  override def computeAndAffectOutputWithPreCompute(value: PreComputeClass, vehicle: Long) = {
    val newValue = value.asInstanceOf[SumOfPreCompute]
    violation(vehicle) := newValue.totalViolation
    contentAtEndOfVehicleRoute(vehicle) := newValue.totalContent
  }

  override def restoreValueAtCheckpoint(value: SavedValuesAtCheckpoint, checkpointLevel: Long) = {
    for (vehicle <- changedVehiclesSinceCheckpoint0.indicesAtTrue) {
      violation(vehicle) := value.violationAtCheckpoint(vehicle)
      contentAtEndOfVehicleRoute(vehicle) := value.contentAtEndOfVehicleRouteAtCheckpoint(vehicle)
    }
    if (checkpointLevel == 0L)
      changedVehiclesSinceCheckpoint0.all = false
  }

  override def valuesToSave(): SavedValuesAtCheckpoint = {
    val currentViolation = Array.tabulate(v)(violation(_).newValue)
    val currentContentAtEnd = Array.tabulate(v)(contentAtEndOfVehicleRoute(_).newValue)
    SavedValuesAtCheckpoint(currentViolation, currentContentAtEnd)
  }

  override def doPreComputeAtCheckpoint0(vehicle: Long,checkpointValue:IntSequence) = {
    val explorerAtVehicleStart = checkpointValue.explorerAtAnyOccurrence(vehicle).head
    val contentAtStart = deltaAtNode(vehicle)
    val preComputeAtStart = PreComputeContainer(Some(vehicle),None, contentAtStart,RedBlackTreeMap(List((contentAtStart, 1L))))
    preComputedValues(vehicle) = preComputeAtStart

    var explorerOpt = explorerAtVehicleStart.next
    var prevNode = vehicle

    while (explorerOpt match {
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if (node >= v) {
          //continuing the same vehicle
          val preComputeAtPrev = preComputedValues(prevNode).asInstanceOf[PreComputeContainer]

          val contentAtNode = preComputeAtPrev.contentAtNodeAtCheckpoint + deltaAtNode(node)

          val rbAtNode = addToReachCount(contentAtNode, preComputeAtPrev.rb)

          val preComputeAtNode = PreComputeContainer(Some(node), Some(prevNode), contentAtNode,rbAtNode)

          preComputedValues(node) = preComputeAtNode

          prevNode = node
          explorerOpt = explorer.next
          true
        } else {
          //at the next vehicle
          false
        }
    }) {}
  }

  override def checkInternals(c: Checker): Unit = {
    for (v <- vehicles) {
      val (viol, contentAtEnd) = computeViolationFromScratchNoPrecompute(routes.value, v)
      c.check(violation(v).value == viol, Some("Error on violation of vehicle " + v + " expected:" + viol + " actual output:" + violation(v).value))
      c.check(contentAtEndOfVehicleRoute(v).value == contentAtEnd, Some("Error on content at end of vehicle route v:" + v + " expected:" + contentAtEnd + " actual output:" + contentAtEndOfVehicleRoute(v).value))
    }
  }

  override def toString: String = {
    "VehicleCapacity(routes: " + routes.name + " n: " + n + " v: " + v + " maxCapacity: " + maxCapacity + "){\n" +
      (0L until v).toList.map((vehicle: Long) => {
        val header = "\tvehicle" + vehicle + " contentAtVehicleEnd: " + contentAtEndOfVehicleRoute(vehicle).newValue + " totalViolation: " + violation(vehicle).newValue + "\n"
        var explorerOpt = routes.value.explorerAtAnyOccurrence(vehicle).get.next
        var contentAtNode = deltaAtNode(vehicle)
        var acc: String = "\tnode: " + f"$vehicle%-7Ld" + "\t" + "deltaAtNode: " + f"${deltaAtNode(vehicle)}%-4Ld" + "\t" + "contentAtNode: " + f"$contentAtNode%-4Ld" + (if (contentAtNode > maxCapacity) "\t" + "violation:" + (contentAtNode - maxCapacity) else "") + "\n"
        var currentRoute = Array(vehicle)
        while (explorerOpt match {
          case None => //at end of last vehicle
            false
          case Some(explorer) if explorer.value < v =>
            //reached another vehicle
            false
          case Some(explorer) if explorer.value >= v =>
            val node = explorer.value
            contentAtNode += deltaAtNode(node)
            acc += "\tnode: " + f"$node%-7Ld" + "\t" + "deltaAtNode: " + f"${deltaAtNode(node)}%-4Ld" + "\t" + "contentAtNode: " + f"$contentAtNode%-4Ld" + (if (contentAtNode > maxCapacity) "\t" + "violation: " + (contentAtNode - maxCapacity) else "") + "\n"
            explorerOpt = explorer.next
            currentRoute = currentRoute :+ node
            true
        }) {}
        header + "\troute: " + currentRoute.mkString("->") + "\n" + acc + "}\n"
      }).mkString("")
  }

}


/**
  * Global class for the pre-computes
  */
abstract class PreComputeClass

/**
  *
  * @param node node associated to the pre-compute
  * @param prevNodeAtCheckpoint0 used for the computation of the violation for a flipped segment
  * @param contentAtNodeAtCheckpoint
  * @param rb RedBlackTreeMap used to compute the integral
  */
case class PreComputeContainer(node: Option[Long],
                               prevNodeAtCheckpoint0: Option[Long],
                               contentAtNodeAtCheckpoint: Long,
                               rb: RedBlackTreeMap[Long] = RedBlackTreeMap.empty[Long]) extends PreComputeClass

/**
  * This class is used to not define a minus operator between two red-black tree.
  * The sub-class extended this class contains information needed for compute the integral
  */
abstract class DeltaOfPreCompute extends PreComputeClass

case class DeltaOfPreComputeForSegment(fromRB: RedBlackTreeMap[Long],
                                       toRB: RedBlackTreeMap[Long],
                                       deltaOfContent: Long,
                                       contentAtPrevNode: Long) extends DeltaOfPreCompute

case class DeltaOfPreComputeForFlippedSegment(fromRB: RedBlackTreeMap[Long],
                                              toRB: RedBlackTreeMap[Long],
                                              deltaOfContent: Long,
                                              contentAtToNode: Long) extends DeltaOfPreCompute

case class DeltaFromScratch(fromNode: Long,
                            toNode: Long) extends DeltaOfPreCompute

/**
  * Like for minus, we don't define a plus between red-black tree.
  * This class contains the information that we really want: the total violation and the content at a node
  * @param totalViolation
  * @param totalContent
  */
case class SumOfPreCompute(totalViolation: Long, totalContent: Long) extends PreComputeClass


case class SavedValuesAtCheckpoint(violationAtCheckpoint: Array[Long],
                                   contentAtEndOfVehicleRouteAtCheckpoint: Array[Long])*/