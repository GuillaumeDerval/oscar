package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

abstract class LogReducedGlobalConstraintWithExtremes[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends LogReducedGlobalConstraint[T,U](routes:ChangingSeqValue,v :Int){

  class NodeAndExtremePreComputes(val node:Int,
                                  var forward:T = null.asInstanceOf[T],
                                  var backward:T = null.asInstanceOf[T]){
    override def toString: String = {
      "NodeAndExtremePreComputes(node:" + node + " forward:" + forward + " backward:" + backward + ")"
    }
  }

  private val vehicleToExtremePrecomputes:Array[Array[NodeAndExtremePreComputes]] = Array.fill(v)(null)


  private def identifyNodesAndAllocateExtremePrecomputes(e:Option[IntSequenceExplorer],
                                                         vehicle:Int,positionInVehicleRoute:Int,
                                                         preComputedVals:Array[VehicleAndPosition]): Unit ={
    e match {
      case None =>
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute)(null)

      case  Some(x) if x.value < v && x.value != vehicle => ;
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute)(null)

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
    val nv = extremePrecomputesOfCurrentVehicle.length

    //forward precompute
    if (nv > 1) {
      extremePrecomputesOfCurrentVehicle(0).forward =
        step(
          extremePrecomputesOfCurrentVehicle(0).node,
          extremePrecomputesOfCurrentVehicle(1).node)
      for (i <- 1 until nv) {
        extremePrecomputesOfCurrentVehicle(i).forward =
          composeSteps(
            extremePrecomputesOfCurrentVehicle(i - 1).forward,
            step(
              extremePrecomputesOfCurrentVehicle(i - 1).node,
              extremePrecomputesOfCurrentVehicle(i).node))
      }

      extremePrecomputesOfCurrentVehicle(nv - 1).backward =
        step(
          extremePrecomputesOfCurrentVehicle(nv - 2).node,
          extremePrecomputesOfCurrentVehicle(nv - 1).node)

      //backward precompute
      for (i <- (0 until nv - 1).reverse) {
        extremePrecomputesOfCurrentVehicle(i).backward =
          composeSteps(
            step(
              extremePrecomputesOfCurrentVehicle(i).node,
              extremePrecomputesOfCurrentVehicle(i+1).node),
            extremePrecomputesOfCurrentVehicle(i+1).backward)
      }
    }
  }

  override def computeVehicleValue(vehicle:Int,
                                   segments:List[Segment[VehicleAndPosition]],
                                   routes:IntSequence,
                                   preComputedVals:Array[VehicleAndPosition]):U = {

    computeVehicleValueComposed(vehicle,decorateSegments(vehicle,segments,isFirst = true))

  }

  def decorateSegments(vehicle:Int, segments:List[Segment[VehicleAndPosition]],isFirst:Boolean):List[LogReducedSegment[T]] = {

    segments match{
      case Nil => Nil
      case head :: tail =>
        (head match{
          case PreComputedSubSequence
            (startNode: Int, startNodeValue: VehicleAndPosition,
            endNode: Int, endNodeValue: VehicleAndPosition) =>

            if(isFirst && endNodeValue.positionInVehicleRoute > startNodeValue.positionInVehicleRoute+1){
              LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () =>
                  List(vehicleToExtremePrecomputes(startNodeValue.vehicle)(endNodeValue.positionInVehicleRoute).forward)
              )
            } else if (tail.isEmpty && startNode != endNode) {
              LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () =>
                  List(vehicleToExtremePrecomputes(startNodeValue.vehicle)(startNodeValue.positionInVehicleRoute).backward)
              )
            }else{
              LogReducedPreComputedSubSequence[T](
                startNode: Int, endNode: Int,
                stepGenerator = () => extractSequenceOfT(
                  startNodeValue.vehicle, startNode, startNodeValue.positionInVehicleRoute,
                  endNode, endNodeValue.positionInVehicleRoute, flipped = false))
            }
          case FlippedPreComputedSubSequence(
          startNode: Int, startNodeValue: VehicleAndPosition,
          endNode: Int, endNodeValue: VehicleAndPosition) =>

            LogReducedFlippedPreComputedSubSequence[T](
              startNode: Int, endNode: Int,
              stepGenerator = () => extractSequenceOfT(
                startNodeValue.vehicle, startNode, startNodeValue.positionInVehicleRoute,
                endNode, endNodeValue.positionInVehicleRoute, flipped = true))

          case NewNode(node: Int) =>
            LogReducedNewNode[T](node: Int)
        }) :: decorateSegments(vehicle:Int, segments = tail,isFirst = false)
    }
  }
}
