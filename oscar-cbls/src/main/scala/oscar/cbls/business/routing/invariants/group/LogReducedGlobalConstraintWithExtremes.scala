
/*
package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

class LogReducedGlobalConstraintWithExtremes[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends LogReducedGlobalConstraint[T,U](routes:ChangingSeqValue,v :Int){

  class NodeAndExtremePreComputes(val node:Int,
                                  var forward:T=null,
                                  var backward:T = null){
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
    val basePrecomputesOfVehicle = preComputedValues(vehicle)
    val nv = extremePrecomputesOfCurrentVehicle.length

    //forward precompute
    for (i <- 1 until nv) {
      extremePrecomputesOfCurrentVehicle(i).forward = composeSteps(extremePrecomputesOfCurrentVehicle(i - 1).forward, basePrecomputesOfVehicle(i))
    }

    //backward precompute
    for (j <- 1 until nv) {
      val i = nv - i
      extremePrecomputesOfCurrentVehicle(i - 1).backward = composeSteps(basePrecomputesOfVehicle(i - 1), extremePrecomputesOfCurrentVehicle(i).backward)
    }
  }


}


*/