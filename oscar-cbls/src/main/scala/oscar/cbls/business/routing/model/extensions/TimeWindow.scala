package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeIntegerIntegerDimensionOnVehicle
import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
class TimeWindow(vrp: VRP,
                 forwardCumulativeIntegerIntegerDimensionOnVehicle: ForwardCumulativeIntegerIntegerDimensionOnVehicle,
                 val earlylines: Array[Int],
                 val deadlines: Array[Int],
                 val taskDurations: Array[Int],
                 val maxWaitingDurations: Array[Int]) extends VRPExtension(vrp){

  private val relevantPredecessorsOfNodes =
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect{
      case predecessor if earlylines(predecessor) <= deadlines(node) && predecessor != node => predecessor
    }:_*)).toMap

  private val relevantSuccessorsOfNode =
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect{
      case successor if deadlines(successor) >= earlylines(node) => successor
    }: _*)).toMap

  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: HashSet[Int]): HashSet[Int] = {
    potentialRelevantNeighbors.intersect(relevantPredecessorsOfNodes(node))
  }

  override def postFilter(node: Int) = (neighbor: Int) => {
    val successor = vrp.routes.value.explorerAtAnyOccurrence(neighbor).head.next
    if(successor.isDefined)
      relevantSuccessorsOfNode(node).contains(successor.get.value)
    else
      true
  }
}

class TimeWindowExtensionBuilder(vrp: VRP, timeInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle){

  private val earlylines: Array[Int] = Array.fill(vrp.n)(Int.MinValue)
  private val deadlines: Array[Int] = Array.fill(vrp.n)(Int.MaxValue)
  private val taskDurations: Array[Int] = Array.fill(vrp.n)(0)
  private val maxWaitingDurations: Array[Int] = Array.fill(vrp.n)(Int.MaxValue)

  def addEarlylines(earlylines: Array[Int]): Unit ={
    for(i <- earlylines.indices)
      this.earlylines(i) = earlylines(i)
  }

  def addDeadlines(deadlines: Array[Int]): Unit ={
    for(i <- deadlines.indices)
      this.deadlines(i) = deadlines(i)
  }

  def addTaskDurations(taskDurations: Array[Int]): Unit ={
    for(i <- taskDurations.indices)
      this.taskDurations(i) = taskDurations(i)
  }

  def addMaxWaitingDurations(maxWaitingDurations: Array[Int]): Unit ={
    for(i <- maxWaitingDurations.indices)
      this.maxWaitingDurations(i) = maxWaitingDurations(i)
  }

  def build(): TimeWindow = {
    new TimeWindow(vrp,timeInvariant,earlylines,deadlines,taskDurations,maxWaitingDurations)
  }
}
