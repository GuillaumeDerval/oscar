package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

abstract class LogReducedGlobalConstraintWithExtremes[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends LogReducedGlobalConstraint[T,U](routes:ChangingSeqValue,v :Int){

  class NodeAndExtremePreComputes(val node:Int,
                                  var fromStart:T = null.asInstanceOf[T],
                                  var toEnd:T = null.asInstanceOf[T]){
    override def toString: String = {
      "NodeAndExtremePreComputes(node:" + node + " fromStart:" + fromStart + " toEnd:" + toEnd + ")"
    }
  }

  private val vehicleToExtremePrecomputes:Array[Array[NodeAndExtremePreComputes]] = Array.fill(v)(null)


  private def identifyNodesAndAllocateExtremePrecomputes(e:Option[IntSequenceExplorer],
                                                         vehicle:Int,positionInVehicleRoute:Int,
                                                         preComputedVals:Array[VehicleAndPosition]): Unit ={
    e match {
      case None =>
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case  Some(x) if x.value < v && x.value != vehicle => ;
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case Some(ex) =>
        identifyNodesAndAllocateExtremePrecomputes(ex.next, vehicle, positionInVehicleRoute + 1, preComputedVals)

        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(ex.value)
    }
  }

  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[VehicleAndPosition]): Unit = {
    super.performPreCompute(vehicle, routes, preComputedVals)
    //also compute the extremes forward and backwards

    identifyNodesAndAllocateExtremePrecomputes(
      routes.explorerAtAnyOccurrence(vehicle),
      vehicle,
      0,
      preComputedVals)

    val extremePrecomputesOfCurrentVehicle = vehicleToExtremePrecomputes(vehicle)
    val nv = extremePrecomputesOfCurrentVehicle.length-1 //return is ignored here, corrected later
    require(nv >= 1, nv)
    //forward precompute

    extremePrecomputesOfCurrentVehicle(0).fromStart = nodeValue(vehicle)

    for (i <- 1 until nv) {
      extremePrecomputesOfCurrentVehicle(i).fromStart =
        composeSteps(
          extremePrecomputesOfCurrentVehicle(i - 1).fromStart,
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node))
    }

    extremePrecomputesOfCurrentVehicle(nv).fromStart =
      composeSteps(
        extremePrecomputesOfCurrentVehicle(nv - 1).fromStart,
        endNodeValue(vehicle))

    extremePrecomputesOfCurrentVehicle(nv).toEnd = endNodeValue(vehicle)

    //backward precompute
    for (i <- (0 until nv).reverse) {
      extremePrecomputesOfCurrentVehicle(i).toEnd =
        composeSteps(
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node),
          extremePrecomputesOfCurrentVehicle(i+1).toEnd)
    }
  }

  override def computeVehicleValue(vehicle:Int,
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

  def decorateSegmentsExtremes(vehicle:Int,
                               segments:List[Segment[VehicleAndPosition]],
                               isFirst:Boolean):List[LogReducedSegment[T]] = {

    //TODO: it seems that the list construction produces a significant overhead. These might be replaced by QLists.
    segments match{
      case Nil =>
        //back to start; we add a single node (this will seldom be used, actually, since back to start is included in PreComputedSubSequence that was not flipped
        List(LogReducedPreComputedSubSequence[T](
          vehicle: Int, vehicle: Int,
          stepGenerator = () => List(endNodeValue(vehicle))))
      case head :: tail =>
        head match{
          case PreComputedSubSequence
            (startNode: Int, startNodeValue: VehicleAndPosition,
            endNode: Int, endNodeValue: VehicleAndPosition) =>

            if(isFirst){
              require(vehicle == startNodeValue.vehicle)
              //println("FROM START EXTREME!")

              LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () =>
                  List(vehicleToExtremePrecomputes(vehicle)(endNodeValue.positionInVehicleRoute).fromStart)
              ) :: decorateSegmentsExtremes(vehicle:Int, segments = tail,isFirst = false)

            } else if(
              startNodeValue.vehicle == vehicle
                && tail.isEmpty //tested second because more time consuming than the first condition
                && endNodeValue.positionInVehicleRoute == vehicleToExtremePrecomputes(vehicle).length-2
                ){
              //last one, on the same vehicle as when pre-computation was performed, and nothing was removed until the end of this route

              //println("TO END EXTREME!")

              List(LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () =>
                  List(vehicleToExtremePrecomputes(vehicle)(startNodeValue.positionInVehicleRoute).toEnd)
              ))
            }else{
              LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () => extractSequenceOfT(
                  startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                  endNodeValue.positionInVehicleRoute, flipped = false)) :: decorateSegmentsExtremes(vehicle:Int, segments = tail,isFirst = false)
            }
          case FlippedPreComputedSubSequence(
          startNode: Int, startNodeValue: VehicleAndPosition,
          endNode: Int, endNodeValue: VehicleAndPosition) =>

            LogReducedFlippedPreComputedSubSequence[T](
              startNode: Int, endNode: Int,
              stepGenerator = () => extractSequenceOfT(
                startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                endNodeValue.positionInVehicleRoute, flipped = true)) :: decorateSegmentsExtremes(vehicle:Int, segments = tail,isFirst = false)

          case NewNode(node: Int) =>
            LogReducedNewNode[T](node: Int, nodeValue(node)) :: decorateSegmentsExtremes(vehicle:Int, segments = tail,isFirst = false)
        }
    }
  }
}
