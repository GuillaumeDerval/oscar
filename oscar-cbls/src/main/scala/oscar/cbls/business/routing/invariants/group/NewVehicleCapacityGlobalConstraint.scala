package oscar.cbls.business.routing.invariants.group

import oscar.cbls.Variable
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.ChangingSeqValue

class NewVehicleCapacityGlobalConstraint(routes : ChangingSeqValue,
                                         nbVehicle : Int,
                                         deltaAtNode : Array[Int],
                                         maxCapacity : Int)
  extends GlobalConstraintDefinition[PrecomputedValues,OutputValues](routes,nbVehicle) {

  def computeValuesForVehicle(explorer : Option[IntSequenceExplorer],
                              preComputedVals : Array[PrecomputedValues],
                              previousLoad : Int,
                              previousRBT : RedBlackTreeMap[Int]) : Unit = {
    explorer match{
      case None =>
      case Some(v) =>
        if (v.value < nbVehicle)
          ()
        else {
          val actualLoad = previousLoad + deltaAtNode(v.value)
          val actualRBT = previousRBT.insert(actualLoad,previousRBT.getOrElse(actualLoad,0) + 1)
          preComputedVals(v.value) = PrecomputedValues(actualRBT)
          computeValuesForVehicle(explorer.get.next,preComputedVals,actualLoad,actualRBT)
        }
    }
  }
  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle         the vehicle where pre-computation must be performed
    * @param routes          the sequence representing the route of all vehicle
    *                        BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param preComputedVals The array of precomputed values
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[PrecomputedValues]): Unit = {
    computeValuesForVehicle(routes.explorerAtAnyOccurrence(vehicle).get.next,preComputedVals,deltaAtNode(vehicle),RedBlackTreeMap(List((deltaAtNode(vehicle),1))))
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle         the vehicle that we are focusing on
    * @param segments        the segments that constitute the route.
    *                        The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes          the sequence representing the route of all vehicle
    * @param preComputedVals The array of precomputed values
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[PrecomputedValues]], routes: IntSequence, preComputedVals: Array[PrecomputedValues]): OutputValues = ???

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: OutputValues): Unit = ???

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): OutputValues = ???
override def outputVariables: Iterable[Variable] = ???
}


case class PrecomputedValues(rbt : RedBlackTreeMap[Int])

case class OutputValues(violation : Int)
