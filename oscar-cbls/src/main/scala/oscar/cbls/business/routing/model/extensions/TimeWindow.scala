package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeIntegerIntegerDimensionOnVehicle
import oscar.cbls.business.routing.model.{TTFMatrix, VRP}

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
class TimeWindow(vrp: VRP,
                 timeInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle,
                 timeMatrix: TTFMatrix,
                 val earlylines: Array[Int],
                 val deadlines: Array[Int],
                 val taskDurations: Array[Int],
                 val maxWaitingDurations: Array[Int],
                 val maxTravelDurations: Map[(Int,Int),Int]) extends VRPExtension(vrp){

  val arrivalTime = (timeInvariant.content1AtEnd ++ timeInvariant.content1AtNode).map(_.value)
  val leaveTime = (timeInvariant.content2AtStart ++ timeInvariant.content2AtNode).map(_.value)

  private val relevantPredecessorsOfNodes =
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect{
      case predecessor if earlylines(predecessor) <= deadlines(node) && predecessor != node => predecessor
    }:_*)).toMap

  private val relevantSuccessorsOfNode =
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect{
      case successor if deadlines(successor) >=
        (earlylines(node) + timeMatrix.getTravelDuration(node,earlylines(node) + taskDurations(node),successor)) => successor
    }: _*)).toMap





  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: List[Int]): List[Int] = {
    potentialRelevantNeighbors.filter(relevantPredecessorsOfNodes(node))
  }

  /**
    * WARNING : Here the neighbor is the after wich we want to insert the node.
    *           So when using the route explorer we must take the next node.
    * @param node The node we want to insert
    * @return A function that takes a neighbor which represent the node after which we want to insert a node
    *         and return true if by inserting at that position we don't violate some constraint of the current successor of neighbor.
    */
  override def postFilter(node: Int) = (neighbor: Int) => {
    val routeExplorer = vrp.routes.value.explorerAtAnyOccurrence(neighbor)
    val successor = if(routeExplorer.isDefined) routeExplorer.get.next else None

    (successor.isDefined && relevantSuccessorsOfNode(node).contains(successor.get.value)) || successor.isEmpty
  }


  private def reduceTimeWindows(): Unit ={
    if(maxTravelDurations.isEmpty) return
    else {
      val keys = maxTravelDurations.keys.toList
      val fromToValue = maxTravelDurations.map(md => md._1._1 -> (md._1._2, md._2))
      val toFromValue = maxTravelDurations.map(md => md._1._2 -> (md._1._1, md._2))

      def startingPositions(): List[Int] = {
        val origins = keys.map(_._1).distinct
        val destinations = keys.map(_._2).distinct

        val startingPositions = vrp.nodes.collect {
          case node if origins.contains(node) && !destinations.contains(node) => node
        }
        require(startingPositions.nonEmpty, "No starting positions in your maxDetours couples. You may have introduce some cycle.")
        startingPositions.toList
      }

      def endingPositions(): List[Int] = {
        val origins = keys.map(_._1).distinct
        val destinations = keys.map(_._2).distinct

        val endingPositions = vrp.nodes.collect {
          case node if !origins.contains(node) && destinations.contains(node) => node
        }
        require(endingPositions.nonEmpty, "No ending positions in your maxDetours couples. You may have introduce some cycle.")
        endingPositions.toList
      }

      for (startPos <- startingPositions()) {
        var currentPost = startPos
        while (fromToValue.get(currentPost).isDefined) {
          val to = fromToValue(currentPost)._1
          val value = fromToValue(currentPost)._2
          deadlines(to) = Math.min(deadlines(to), deadlines(currentPost) + value + taskDurations(to))
          currentPost = to
        }
      }

      for (endPos <- endingPositions()) {
        var currentPost = endPos
        while (toFromValue.get(currentPost).isDefined) {
          val from = toFromValue(currentPost)._1
          val value = toFromValue(currentPost)._2
          earlylines(from) = Math.min(earlylines(from), earlylines(currentPost) + value + taskDurations(from))
          currentPost = from
        }
      }
    }
  }

  reduceTimeWindows()
}

class TimeWindowExtensionBuilder(vrp: VRP, timeInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle, timeMatrix: TTFMatrix){

  private val earlylines: Array[Int] = Array.fill(vrp.n)(Int.MinValue)
  private val deadlines: Array[Int] = Array.fill(vrp.n)(Int.MaxValue)
  private val taskDurations: Array[Int] = Array.fill(vrp.n)(0)
  private val maxWaitingDurations: Array[Int] = Array.fill(vrp.n)(Int.MaxValue)
  private var maxTravelDurations: Map[(Int,Int), Int] = Map.empty

  def addEarlylines(earlylines: Array[Int]): TimeWindowExtensionBuilder ={
    for(i <- earlylines.indices)
      this.earlylines(i) = earlylines(i)
    this
  }

  def addDeadlines(deadlines: Array[Int]): TimeWindowExtensionBuilder ={
    for(i <- deadlines.indices)
      this.deadlines(i) = deadlines(i)
    this
  }

  def addTaskDurations(taskDurations: Array[Int]): TimeWindowExtensionBuilder ={
    for(i <- taskDurations.indices)
      this.taskDurations(i) = taskDurations(i)
    this
  }

  def addMaxWaitingDurations(maxWaitingDurations: Array[Int]): TimeWindowExtensionBuilder ={
    for(i <- maxWaitingDurations.indices)
      this.maxWaitingDurations(i) = maxWaitingDurations(i)
    this
  }


  def addMaxTravelDurations(maxTravelDurations: Map[(Int,Int),Int]): TimeWindowExtensionBuilder ={
    this.maxTravelDurations = maxTravelDurations
    this
  }

  def build(): TimeWindow = {
    new TimeWindow(vrp,timeInvariant,timeMatrix,earlylines,deadlines,taskDurations,maxWaitingDurations,maxTravelDurations)
  }
}
