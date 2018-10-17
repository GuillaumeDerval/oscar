package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

class VehicleAndPosition(val vehicle:Int, val position:Int)

abstract class ReducedGlobalConstraint[T:Manifest,U:Manifest](routes:ChangingSeqValue,v :Int)
  extends GlobalConstraintDefinition[VehicleAndPosition,U](routes,v){

  class NodeAndPreComputes(val node:Int,var precomputes:Array[T] = null)
  val vehicleToPrecomputes:Array[Array[NodeAndPreComputes]] = Array.fill(v)(null)

  def performPreCompute(vehicle:Int,
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


  def identifyNodesAndAllocate(e:Option[IntSequenceExplorer],
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

  def decomposeToBitNumbersMSBFirst(x:Int):List[Int] = {
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

  def decorateAndAllocate(vehicle:Int,positionInRoute:Int,level:Int,allocateFirst:Boolean){
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

  def computeVehicleValue(vehicle:Int,
                          segments:List[Segment[VehicleAndPosition]],
                          routes:IntSequence,
                          preComputedVals:Array[VehicleAndPosition]):U = {

    computeVehicleValueComposed(vehicle,
      composedSegments = segments.map(
        _ match {
          case PreComputedSubSequence(startNode: Int, startNodeValue: VehicleAndPosition, endNode: Int, endNodeValue: VehicleAndPosition) =>
            PreComputedSubSequenceComposed[T](
              startNode:Int, endNode:Int,
              chain = extractSequenceOfT(startNodeValue.vehicle,startNode,startNodeValue.position,endNode,endNodeValue.position,flipped=false))

          case FlippedPreComputedSubSequence(startNode:Int,startNodeValue:VehicleAndPosition, endNode:Int, endNodeValue:VehicleAndPosition) =>
            FlippedPreComputedSubSequenceComposed[T](
              startNode:Int, endNode:Int,
              chain = extractSequenceOfT(startNodeValue.vehicle,startNode,startNodeValue.position,endNode,endNodeValue.position,flipped=true))

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


  def extractSequenceOfTUnflipped(vehicle:Int,
                                  startPositionInRoute:Int,
                                  endPositionInRoute:Int):List[T] = {

    extractSequenceOfTUnflippedGoingUp(vehicleToPrecomputes(vehicle),
      startPositionInRoute:Int,
      endPositionInRoute:Int)
  }

  def extractSequenceOfTUnflippedGoingUp(vehiclePreComputes:Array[NodeAndPreComputes],
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

  def extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
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
                                             chain:List[T]) extends SegmentWithComposedFunction[T]{
  override def toString: String = {
    "PreComputedSubSequence(startNode:" + startNode + " endNode:" + endNode + " chain:" + chain.mkString(",") + ")"
  }
}

case class FlippedPreComputedSubSequenceComposed[T](startNode:Int,
                                                    endNode:Int,
                                                    chain:List[T]) extends SegmentWithComposedFunction[T]{
  override def toString: String = {
    "PreComputedFLIPPEDSubSequence(startNode:" + startNode + " endNode:" + endNode + " chain:" + chain.mkString(",") + ")"
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

