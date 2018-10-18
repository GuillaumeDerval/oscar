package oscar.cbls.business.routing.invariants.group

import oscar.cbls.{CBLSIntVar, Variable}
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

class VehicleAndPosition(val vehicle:Int, val position:Int)


case class NodesOnSubsequence(nbNodes:Int)

class NumberOfNodes(routes:ChangingSeqValue, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends ReducedGlobalConstraint[NodesOnSubsequence,Int](routes,v){
  /**
    * this method delivers the value of stepping from node "fromNode" to node "toNode.
    * you can consider that these two nodes are adjacent.
    *
    * @param fromNode
    * @param toNode
    * @return the type T associated with the step "fromNode -- toNode"
    */
  override def step(fromNode: Int, toNode: Int): NodesOnSubsequence = {
    NodesOnSubsequence(2)
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated wit hthe first step followed by the second step
    */
  override def composeSteps(firstStep: NodesOnSubsequence, secondStep: NodesOnSubsequence): NodesOnSubsequence =
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes)

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int, segments: List[SegmentWithComposedFunction[NodesOnSubsequence]]): Int = {
    segments.map({
        case PreComputedSubSequenceComposed(startNode, endNode, chain)=>
          chain.map(_.nbNodes).sum
        case FlippedPreComputedSubSequenceComposed(startNode, endNode, chain) =>
          chain.map(_.nbNodes).sum
        case NewNodeComposed(node) => 1
      }).sum
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Int): Unit = {
    nbNodesPerRoute(vehicle) := value
  }

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Int = ???

  override def outputVariables: Iterable[Variable] = nbNodesPerRoute
}




/**
  * This API provides an easy to use framework for defining a custom global constraint for vehicle routing.
  * it is to be used when the pre-computation needs to be performed on every possible subsequence of route,
  * thus pre-computationn is O(n²)-time to achieve O(1) query time per segment.
  *
  * This particular API provides a reduction of the pre-computation time to O(n)
  * at the cot of performing the segment query in O(log(n))
  *
  * The difference in implementation is that it not decorates evey possible segment with a value of type T.
  * Instead it decorates a subset of these segments, and each queried segment has a sequence of values of type T associated with it.
  * these values are queried from the pre-computation.
  * The assembly includes O(log(n)) of these pre-computations.
  *
  * @param routes the route of vehicles
  * @param v the number of vehicles
  * @tparam T the type of pre-computation, which is on subsequences (not on nodes)
  * @tparam U the output type of the algorithms, that you need to assign to the output variables
  */
abstract class ReducedGlobalConstraint[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends ReducedGlobalConstraintAlgo [T,U](routes:ChangingSeqValue,v :Int){

  /**
    * this method delivers the value of stepping from node "fromNode" to node "toNode.
    * you can consider that these two nodes are adjacent.
    * @param fromNode
    * @param toNode
    * @return the type T associated with the step "fromNode -- toNode"
    */
  override def step(fromNode: Int, toNode: Int): T

  /**
    * this method is for composing steps into bigger steps.
    * @param firstStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated wit hthe first step followed by the second step
    */
  override def composeSteps(firstStep: T, secondStep: T): T

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int,
                                           segments: List[SegmentWithComposedFunction[T]]): U

}


abstract class ReducedGlobalConstraintAlgo[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends GlobalConstraintDefinition[VehicleAndPosition,U](routes,v){

  class NodeAndPreComputes(val node:Int,var precomputes:Array[T] = null)
  val vehicleToPrecomputes:Array[Array[NodeAndPreComputes]] = Array.fill(v)(null)

  override protected final def performPreCompute(vehicle:Int,
                                                 routes:IntSequence,
                                                 preComputedVals:Array[VehicleAndPosition]): Unit ={

    //identify all nodes
    identifyNodesAndAllocate(routes.explorerAtAnyOccurrence(vehicle),vehicle,0,preComputedVals)

    var sequenceOfLevels = decomposeToBitNumbersMSBFirst(vehicleToPrecomputes(vehicle).length)

    var positionInRoute = 0
    while(sequenceOfLevels.nonEmpty){
      val currentLevel = sequenceOfLevels.head
      sequenceOfLevels = sequenceOfLevels.tail

      decorateAndAllocate(vehicle,positionInRoute,currentLevel,allocateFirst=true)
      positionInRoute += 1 << currentLevel
    }
    require(positionInRoute == vehicleToPrecomputes(vehicle).length)
  }


  private def identifyNodesAndAllocate(e:Option[IntSequenceExplorer],
                                       vehicle:Int,positionInVehicleRoute:Int,
                                       preComputedVals:Array[VehicleAndPosition]): Unit ={
    e match {
      case None | Some(x) if x.value < v && x.value != vehicle => ;
        //end
        vehicleToPrecomputes(vehicle) = Array.fill(positionInVehicleRoute)(null)

      case Some(ex) =>
        preComputedVals(ex.value) = new VehicleAndPosition(vehicle, positionInVehicleRoute)

        identifyNodesAndAllocate(ex.next, vehicle, positionInVehicleRoute + 1, preComputedVals)

        vehicleToPrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndPreComputes(ex.value)
    }
  }

  private def decomposeToBitNumbersMSBFirst(x:Int):List[Int] = {
    require(x >= 0)

    var remaining = x
    var offset = 1
    var toReturn = List.empty[Int]

    while(remaining != 0){
      if((remaining & 1) != 0) {
        toReturn = offset :: toReturn
      }
      remaining = remaining >> 1
      offset = offset + 1
    }
    toReturn
  }

  def step(fromNode:Int,toNode:Int):T

  def composeSteps(firstStep:T,secondStep:T):T

  private def decorateAndAllocate(vehicle:Int,positionInRoute:Int,level:Int,allocateFirst:Boolean){
    if(allocateFirst){
      vehicleToPrecomputes(vehicle)(positionInRoute).precomputes = Array.fill(level)(null.asInstanceOf[T])
    }

    if(level == 0){
      vehicleToPrecomputes(vehicle)(positionInRoute).precomputes(0) =
        step(
          vehicleToPrecomputes(vehicle)(positionInRoute).node,
          vehicleToPrecomputes(vehicle)(positionInRoute+1).node)
    }else{

      val stepSize = 1 << level

      decorateAndAllocate(vehicle,positionInRoute,level-1,allocateFirst = false)
      decorateAndAllocate(vehicle, positionInRoute+stepSize,level-1,allocateFirst=true)

      vehicleToPrecomputes(vehicle)(positionInRoute).precomputes(level) =
        composeSteps(
          vehicleToPrecomputes(vehicle)(positionInRoute).precomputes(level-1),
          vehicleToPrecomputes(vehicle)(positionInRoute + stepSize).precomputes(level-1))
    }
  }

  override protected final def computeVehicleValue(vehicle:Int,
                                                   segments:List[Segment[VehicleAndPosition]],
                                                   routes:IntSequence,
                                                   preComputedVals:Array[VehicleAndPosition]):U = {

    computeVehicleValueComposed(vehicle,
      composedSegments = segments.map(
        _ match {
          case PreComputedSubSequence(startNode: Int, startNodeValue: VehicleAndPosition, endNode: Int, endNodeValue: VehicleAndPosition) =>
            PreComputedSubSequenceComposed[T](
              startNode:Int, endNode:Int,
              steps = extractSequenceOfT(startNodeValue.vehicle,startNode,startNodeValue.position,endNode,endNodeValue.position,flipped=false))

          case FlippedPreComputedSubSequence(startNode:Int,startNodeValue:VehicleAndPosition, endNode:Int, endNodeValue:VehicleAndPosition) =>
            FlippedPreComputedSubSequenceComposed[T](
              startNode:Int, endNode:Int,
              steps = extractSequenceOfT(startNodeValue.vehicle,startNode,startNodeValue.position,endNode,endNodeValue.position,flipped=true))

          case NewNode(node:Int) =>
            NewNodeComposed[T](node:Int)
        }
      ))
  }

  def extractSequenceOfT(vehicle:Int,
                         startNode:Int,
                         startPositionInRoute:Int,
                         endNode:Int,
                         endPositionInRoute:Int,
                         flipped:Boolean):List[T] = {

    if(flipped){
      extractSequenceOfTUnflipped(vehicle:Int,
        endPositionInRoute:Int,
        startPositionInRoute:Int).reverse
    }else{
      extractSequenceOfTUnflipped(vehicle:Int,
        startPositionInRoute:Int,
        endPositionInRoute:Int)
    }
  }


  private def extractSequenceOfTUnflipped(vehicle:Int,
                                          startPositionInRoute:Int,
                                          endPositionInRoute:Int):List[T] = {

    extractSequenceOfTUnflippedGoingUp(vehicleToPrecomputes(vehicle),
      startPositionInRoute:Int,
      endPositionInRoute:Int)
  }

  private def extractSequenceOfTUnflippedGoingUp(vehiclePreComputes:Array[NodeAndPreComputes],
                                                 startPositionInRoute:Int,
                                                 endPositionInRoute:Int):List[T] = {


    val maxLevel = vehiclePreComputes(startPositionInRoute).precomputes.length
    val levelStep = 1 << maxLevel

    if(startPositionInRoute + levelStep > endPositionInRoute){
      //we need to go down
      extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
        startPositionInRoute:Int,
        endPositionInRoute:Int,
        maxLevel-1)
    }else{
      //take the step and go up
      vehiclePreComputes(startPositionInRoute).precomputes(maxLevel) ::
        extractSequenceOfTUnflippedGoingUp(vehiclePreComputes:Array[NodeAndPreComputes],
          startPositionInRoute:Int,
          endPositionInRoute:Int)
    }
  }

  private def extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
                                                   startPositionInRoute:Int,
                                                   endPositionInRoute:Int,
                                                   maxLevel:Int):List[T] = {

    val levelStep = 1 << maxLevel

    if(startPositionInRoute + levelStep > endPositionInRoute) {
      //too far, go down further
      extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
        startPositionInRoute:Int,
        endPositionInRoute:Int,
        maxLevel-1)
    }else{
      require(startPositionInRoute + levelStep == endPositionInRoute)
      List(vehiclePreComputes(startPositionInRoute).precomputes(maxLevel))
    }
  }

  def computeVehicleValueComposed(vehicle:Int,
                                  composedSegments:List[SegmentWithComposedFunction[T]]):U


}

sealed abstract class SegmentWithComposedFunction[T]()

case class PreComputedSubSequenceComposed[T](startNode:Int,
                                             endNode:Int,
                                             steps:List[T]) extends SegmentWithComposedFunction[T]{
  override def toString: String = {
    "PreComputedSubSequence(startNode:" + startNode + " endNode:" + endNode + " steps:" + steps.mkString(",") + ")"
  }
}

case class FlippedPreComputedSubSequenceComposed[T](startNode:Int,
                                                    endNode:Int,
                                                    steps:List[T]) extends SegmentWithComposedFunction[T]{
  override def toString: String = {
    "PreComputedFLIPPEDSubSequence(startNode:" + startNode + " endNode:" + endNode + " chain:" + steps.mkString(",") + ")"
  }
}

/**
  * This represent that a node that was not present in the initial sequence when pre-computation was performed.
  * @param node
  */
case class NewNodeComposed[T](node:Int) extends SegmentWithComposedFunction[T]{
  override def toString: String = {
    "NewNode - Node : " + node
  }
}

