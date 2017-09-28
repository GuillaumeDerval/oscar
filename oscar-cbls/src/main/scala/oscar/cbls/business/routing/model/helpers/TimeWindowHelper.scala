package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.TTFMatrix

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/09/17.
  */
object TimeWindowHelper{

  /**
    * This method is used to precompute the relevant predecessors of all the nodes of the problem.
    * Using this you can filter a lot of useless predecessors.
    *
    * (call it only once ;) )
    *
    * A node x is a relevant predecessor of another node y if
    *   earlyline(x) +
    *   taskDurations(x) +
    *   timeMatrix.getTravelDuration(x, earlylines(x) + taskDurations(x), y) +
    *   taskDurations(y) <= deadline(y)
    * Meaning if we can start the task at node x, finish it, travel to y,
    * and finish the task at y before the deadline of y, then x is a relevant neighbor of y.
    *
    * All these informations are used to define the problem, therefore they are static.
    * So we can use them to precompute the relevant predecessors.
    *
    * @param vrp The vehicle routing problem
    * @param earlylines The earlyline of all nodes (meaning, we can't start the task at node x before (earlyline(x))
    * @param deadlines The deadline of all nodes (meaning, the task at node x must be finished before (earlyline(x))
    * @return true if the node is relevant
    */
  def relevantPredecessorsOfNodes(vrp: VRP,
                                  earlylines: Array[Int],
                                  deadlines: Array[Int],
                                  taskDurations: Array[Int],
                                  timeMatrix: TTFMatrix): Map[Int,HashSet[Int]] =
    Array.tabulate(vrp.n)(node => node -> HashSet(vrp.nodes.collect{
      case predecessor if earlylines(predecessor) <= deadlines(node) && predecessor != node => predecessor
    }:_*)).toMap


  /**
    * This method is meant to post-filter a relevant predecessor.
    *
    * A node x is an open predecessor of another node y if (considering z, the current next node of x)
    *   earlyline(y) +
    *   taskDurations(y) +
    *   timeMatrix.getTravelDuration(y, earlylines(y) + taskDurations(y), z) +
    *   taskDurations(z) <= deadline(z)
    *
    * e.g : You have a list of relevant predecessor for a node y but inserting this node y
    * could delayed the arrival time to the current next node of the relevant predecessor.
    *
    *
    * Meaning if we can start the task at node y, finish it, travel to z,
    * and finish the task at z before the deadline of z, then x is an open relevant neighbor of y.
    *
    * All these informations are used to define the problem, therefore they are static.
    * The only information that's not static is the current next of the relevant predecessor
    *
    * @param vrp The vehicle routing problem
    * @param earlylines The earlyline of all nodes (meaning, we can't start the task at node x before (earlyline(x))
    * @param deadlines The deadline of all nodes (meaning, the task at node x must be finished before (earlyline(x))
    * @return true if the node is open
    */
  def isPredecessorOpen(vrp: VRP,
                   earlylines: Array[Int],
                   deadlines: Array[Int],
                   taskDurations: Array[Int],
                   timeMatrix: TTFMatrix)
                  (node: Int,
                   neighbor: Int): Boolean = {
    val routeExplorer = vrp.routes.value.explorerAtAnyOccurrence(neighbor)
    val successor = if(routeExplorer.isDefined) routeExplorer.get.next else None

    successor match{
      case Some(x) =>
        deadlines(x.value) >= (earlylines(node) + timeMatrix.getTravelDuration(node, earlylines(node) + taskDurations(node), x.value))
      case None => true
    }
  }


  /**
    * This method is used to restraint the time window of the nodes (earlylines and deadlines)
    * This restriction is based on the maxTravelDurations values.
    *
    * NOTE : Use it before the instantiation of the time constraint
    *
    * @param vrp The vehicle routing problem
    * @param maxTravelDurations A map (from,to) -> value representing the max travel duration from from to to
    * @param earlylines The earlyline of all nodes (meaning, we can't start the task at node x before (earlyline(x))
    * @param deadlines The deadline of all nodes (meaning, the task at node x must be finished before (earlyline(x))
    * @param taskDurations The task duration of all nodes
    */
  def reduceTimeWindows(vrp: VRP,
                        maxTravelDurations: Map[(Int,Int),Int],
                        earlylines: Array[Int],
                        deadlines: Array[Int],
                        taskDurations: Array[Int]): Unit ={
    if(maxTravelDurations.nonEmpty){
      val keys = maxTravelDurations.keys.toList
      val fromToValue = maxTravelDurations.map(md => md._1._1 -> (md._1._2, md._2))
      val toFromValue = maxTravelDurations.map(md => md._1._2 -> (md._1._1, md._2))

      /**
        * We compute the starting node of each sequence of maxTravelDurations.
        *
        * e.g.: Map((0,1) -> 20, (1,2) -> 40, (1,3) -> 30)     => the starting node is 0
        * @return A list of starting nodes
        */
      def startingNodes(): List[Int] = {
        val origins = keys.map(_._1).distinct
        val destinations = keys.map(_._2).distinct

        val startingNodes = vrp.nodes.collect {
          case node if origins.contains(node) && !destinations.contains(node) => node
        }
        require(startingNodes.nonEmpty, "No starting nodes in your maxDetours couples. You may have introduce some cycle.")
        startingNodes.toList
      }

      /**
        * We compute the starting node of each sequence of maxTravelDurations.
        *
        * e.g.: Map((0,1) -> 20, (1,2) -> 40, (1,3) -> 30)     => the ending nodes are 2 and 3
        * @return A list of ending nodes
        */
      def endingNodes(): List[Int] = {
        val origins = keys.map(_._1).distinct
        val destinations = keys.map(_._2).distinct

        val endingNodes = vrp.nodes.collect {
          case node if !origins.contains(node) && destinations.contains(node) => node
        }
        require(endingNodes.nonEmpty, "No ending nodes in your maxDetours couples. You may have introduce some cycle.")
        endingNodes.toList
      }

      for (startNode <- startingNodes()) {
        var currentNode = startNode
        while (fromToValue.get(currentNode).isDefined) {
          val to = fromToValue(currentNode)._1
          val value = fromToValue(currentNode)._2
          deadlines(to) = Math.min(deadlines(to), deadlines(currentNode) + value + taskDurations(to))
          currentNode = to
        }
      }

      for (endNode <- endingNodes()) {
        var currentNode = endNode
        while (toFromValue.get(currentNode).isDefined) {
          val from = toFromValue(currentNode)._1
          val value = toFromValue(currentNode)._2
          earlylines(from) = Math.min(earlylines(from), earlylines(currentNode) + value + taskDurations(from))
          currentNode = from
        }
      }
    }
  }
}
