package oscar.cbls.lib.invariant.graph.test

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.computation.{CBLSSetVar, ChangingSetValue}
import oscar.cbls.lib.invariant.graph.{ConditionalGraph, VoronoiZonesInvariant}

import scala.collection.immutable.{SortedMap, SortedSet}

class Test extends App{

  val mapSide = 1000
val nbConditionalEdges = 100
  val graph = RandomGraphGenerator
    .generatePseudoPlanarConditionalGraph(
      nbNodes=100,
      nbConditionalEdges=nbConditionalEdges,
      nbNonConditionalEdges=100,
      mapSide)

  val conditions = 0 until nbConditionalEdges
  val possibleCentroids = 0 to 10

  val centroids = new CBLSSetVar(m,SortedSet.empty,possibleCentroids,"centroids")
  val trackedNodes = 0 to 100

  val openConditions = new CBLSSetVar(m,SortedSet.empty,conditions,"openConditions")

  val trackedNodeToDistanceAndCentroidMap = SortedMap.empty[Int,(CBLSIntVar,CBLSIntVar)] ++ trackedNodes.map(nodeID =>
  nodeID -> (new CBLSIntVar(m,0,0 to Integer.MAX_VALUE,"distanceForNode_" + nodeID),
      new CBLSIntVar(m,0,0 to 10,"centroidForNode" + nodeID)))


  new VoronoiZonesInvariant(graph,
    openConditions,
    centroids,
    trackedNodeToDistanceAndCentroidMap)

}
