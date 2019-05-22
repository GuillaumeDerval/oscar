package oscar.cbls.business.routing.invariants.group

import oscar.cbls
import oscar.cbls.{CBLSIntVar, Variable}
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

case class PreComputedDistances(distanceFromStart:Long,
                                distanceToStart:Long)

@deprecated("needs testing","")
class RouteLength(routes:ChangingSeqValue, v:Int, vehicleToRouteLength:Array[CBLSIntVar], assymetricDistance:(Long,Long)=>Long)
  extends GlobalConstraintDefinition[PreComputedDistances,Long](routes,v){


  override def outputVariables: Iterable[Variable] = vehicleToRouteLength

  override def performPreCompute(vehicle: Long, routes: IntSequence, preComputedVals: Array[PreComputedDistances]): Unit = {

    var previousNode = vehicle
    var prevPreComputedValue =PreComputedDistances(0,0)
    preComputedVals(cbls.longToInt(vehicle)) = prevPreComputedValue

    var currentExplorerOPt:Option[IntSequenceExplorer] = routes.explorerAtAnyOccurrence(vehicle).get.next
    //We start at the first node after vehicle start

    while(currentExplorerOPt match{
      case None =>
        //we are at the end of the last route
        false
      case Some(explorer) =>
        if(explorer.value == vehicle+1){
          //we start the next vehicle
          false
        }else{
          //we are not starting the next vehicle, just continue on the current one
          //We tag the current node with the proper value accumulatin on gthe previous node

          prevPreComputedValue = PreComputedDistances(
            distanceFromStart = prevPreComputedValue.distanceFromStart + assymetricDistance(previousNode,explorer.value),
            distanceToStart = prevPreComputedValue.distanceToStart + assymetricDistance(explorer.value,previousNode))

          previousNode = explorer.value
          preComputedVals(cbls.longToInt(explorer.value)) = prevPreComputedValue
          currentExplorerOPt = explorer.next
          true
        }
    }){}

  }

  override def computeVehicleValue(vehicle: Long,
                                   segments: List[Segment[PreComputedDistances]],
                                   routes: IntSequence,
                                   preComputedVals: Array[PreComputedDistances]): Long = {

    def digestListOfSegments(segments: List[Segment[PreComputedDistances]], prevNode: Long): Long = {
      segments match {
        case Nil =>
          //return home
          assymetricDistance(prevNode,vehicle)
        case head :: tail =>
          head match {
            case PreComputedSubSequence(startNode, startNodeValue, endNode, endNodeValue, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,startNode)
              val lengthOfThisSegment = endNodeValue.distanceFromStart - startNodeValue.distanceFromStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case FlippedPreComputedSubSequence(startNode, startNodeValue, endNode, endNodeValue, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,startNode)
              val lengthOfThisSegment = startNodeValue.distanceToStart - endNodeValue.distanceToStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case NewNode(node) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,node)
              distanceToEnterThisSegment + digestListOfSegments(tail, node)
          }
      }
    }

    digestListOfSegments(segments, -1)
  }

  override def assignVehicleValue(vehicle: Long, value: Long): Unit = {
    vehicleToRouteLength(cbls.longToInt(vehicle)) := value
  }


  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence): Long = {
    var previousNode = vehicle
    var toReturn:Long = 0

    var currentExplorerOPt:Option[IntSequenceExplorer] = routes.explorerAtAnyOccurrence(vehicle).get.next
    //We start at the first node after vehicle start

    while(currentExplorerOPt match{
      case None =>
        //we are at the end of the last route
        false
      case Some(explorer) =>
        if(explorer.value == vehicle+1){
          //we start the next vehicle
          false
        }else{
          //we are not starting the next vehicle, just continue on the current one
          toReturn += assymetricDistance(previousNode,explorer.value)
          previousNode = explorer.value
          currentExplorerOPt = explorer.next
          true
        }
    }){}

    toReturn + assymetricDistance(previousNode,vehicle) //return
  }
}
