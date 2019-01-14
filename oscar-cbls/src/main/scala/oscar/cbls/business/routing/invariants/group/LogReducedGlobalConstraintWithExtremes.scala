package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

abstract class LogReducedGlobalConstraintWithExtremes[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Long)
  extends LogReducedGlobalConstraint[T,U](routes:ChangingSeqValue,v :Long){

  class NodeAndExtremePreComputes(val node:Long,
                                  var fromStart:T = null.asInstanceOf[T],
                                  var toEnd:T = null.asInstanceOf[T]){
    override def toString: String = {
      "NodeAndExtremePreComputes(node:" + node + " fromStart:" + fromStart + " toEnd:" + toEnd + ")"
    }
  }

  private val vehicleToExtremePrecomputes:Array[Array[NodeAndExtremePreComputes]] = Array.fill(v)(null)


  private def identifyNodesAndAllocateExtremePrecomputes(e:Option[IntSequenceExplorer],
                                                         vehicle:Long,positionInVehicleRoute:Long,
                                                         preComputedVals:Array[VehicleAndPosition]): Unit ={
    e match {
      case None =>
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1L)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case  Some(x) if x.value < v && x.value != vehicle => ;
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1L)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case Some(ex) =>
        identifyNodesAndAllocateExtremePrecomputes(ex.next, vehicle, positionInVehicleRoute + 1L, preComputedVals)

        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(ex.value)
    }
  }

  override def performPreCompute(vehicle: Long, routes: IntSequence, preComputedVals: Array[VehicleAndPosition]): Unit = {
    super.performPreCompute(vehicle, routes, preComputedVals)
    //also compute the extremes forward and backwards

    identifyNodesAndAllocateExtremePrecomputes(
      routes.explorerAtAnyOccurrence(vehicle),
      vehicle,
      0L,
      preComputedVals)

    val extremePrecomputesOfCurrentVehicle = vehicleToExtremePrecomputes(vehicle)
    val nv = extremePrecomputesOfCurrentVehicle.length-1L //return is ignored here, corrected later
    require(nv >= 1L, nv)
    //forward precompute

    extremePrecomputesOfCurrentVehicle(0L).fromStart = nodeValue(vehicle)

    for (i <- 1L until nv) {
      extremePrecomputesOfCurrentVehicle(i).fromStart =
        composeSteps(
          extremePrecomputesOfCurrentVehicle(i - 1L).fromStart,
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node))
    }

    extremePrecomputesOfCurrentVehicle(nv).fromStart =
      composeSteps(
        extremePrecomputesOfCurrentVehicle(nv - 1L).fromStart,
        endNodeValue(vehicle))

    extremePrecomputesOfCurrentVehicle(nv).toEnd = endNodeValue(vehicle)

    //backward precompute
    for (i <- (0L until nv).reverse) {
      extremePrecomputesOfCurrentVehicle(i).toEnd =
        composeSteps(
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node),
          extremePrecomputesOfCurrentVehicle(i+1L).toEnd)
    }
  }

  override def computeVehicleValue(vehicle:Long,
                                   segments:List[Segment[VehicleAndPosition]],
                                   routes:IntSequence,
                                   preComputedVals:Array[VehicleAndPosition]):U = {

    computeVehicleValueComposed(
      vehicle,
      decorateSegmentsExtremes(
        vehicle,
        segments,
        isFirst = true))

  }

  def decorateSegmentsExtremes(vehicle:Long,
                               segments:List[Segment[VehicleAndPosition]],
                               isFirst:Boolean):QList[LogReducedSegment[T]] = {

    //TODO: it seems that the list construction produces a significant overhead. These might be replaced by QLists.
    segments match{
      case Nil =>
        //back to start; we add a single node (this will seldom be used, actually, since back to start is included in PreComputedSubSequence that was not flipped
        QList(LogReducedPreComputedSubSequenceGiven[T](
          vehicle: Long, vehicle: Long,
          QList(endNodeValue(vehicle))))
      case head :: tail =>
        head match{
          case PreComputedSubSequence
            (startNode: Long, startNodeValue: VehicleAndPosition,
            endNode: Long, endNodeValue: VehicleAndPosition) =>

            if(isFirst){
              require(vehicle == startNodeValue.vehicle)
              //println("FROM START EXTREME!")

              QList(LogReducedPreComputedSubSequenceGiven[T](
                startNode: Long, endNode: Long,
                QList(vehicleToExtremePrecomputes(vehicle)(endNodeValue.positionInVehicleRoute).fromStart)
              ), decorateSegmentsExtremes(vehicle:Long, segments = tail,isFirst = false))

            } else if(
              startNodeValue.vehicle == vehicle
                && tail.isEmpty //tested second because more time consuming than the first condition
                && endNodeValue.positionInVehicleRoute == vehicleToExtremePrecomputes(vehicle).length-2L
            ){
              //last one, on the same vehicle as when pre-computation was performed, and nothing was removed until the end of this route

              //println("TO END EXTREME!")

              QList(LogReducedPreComputedSubSequenceGiven[T](
                startNode: Long, endNode: Long,
                QList(vehicleToExtremePrecomputes(vehicle)(startNodeValue.positionInVehicleRoute).toEnd)
              ))
            }else{
              QList(LogReducedPreComputedSubSequenceLazy[T](
                startNode: Long, endNode: Long,
                stepGenerator = () => extractSequenceOfT(
                  startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                  endNodeValue.positionInVehicleRoute, flipped = false)), decorateSegmentsExtremes(vehicle:Long, segments = tail,isFirst = false))
            }
          case FlippedPreComputedSubSequence(
          startNode: Long, startNodeValue: VehicleAndPosition,
          endNode: Long, endNodeValue: VehicleAndPosition) =>

            QList(LogReducedFlippedPreComputedSubSequence[T](
              startNode: Long, endNode: Long,
              stepGenerator = () => extractSequenceOfT(
                startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                endNodeValue.positionInVehicleRoute, flipped = true)), decorateSegmentsExtremes(vehicle:Long, segments = tail,isFirst = false))

          case NewNode(node: Long) =>
            QList(LogReducedNewNode[T](node: Long, nodeValue(node)), decorateSegmentsExtremes(vehicle:Long, segments = tail,isFirst = false))
        }
    }
  }
}
