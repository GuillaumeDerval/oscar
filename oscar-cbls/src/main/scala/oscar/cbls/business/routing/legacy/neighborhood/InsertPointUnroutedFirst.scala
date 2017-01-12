/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 *     Refactored with respect to the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.business.routing.legacy.neighborhood

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.business.routing.legacy.model.VRP

/**
 * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
 * where u is the numberof unrouted points, and n is the number of routed points
 * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
 * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
 * @param relevantNeighbors a function that, for each unrouted node gives a list of routed node
 *                          such that it is relevant to insert the unrouted node after this routed node
 * @param vrp the routing problem
 * @param neighborhoodName the name of this neighborhood
 * @param best should we search for the best move or the first move?
 * @param hotRestart set to true fo a hot restart fearture on the node to insert
 * @param nodeSymmetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestartOnNextSymmetryClass when you have symmetries among points to insert and hotRestart,
 *                                  this option will try to have the hotRestart starting
 *                                  at a different symmetry class than the last one.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
case class InsertPointUnroutedFirst(unroutedNodesToInsert: () => Iterable[Int],
                       relevantNeighbors: () => Int => Iterable[Int],
                       override val vrp: VRP,
                       neighborhoodName: String = "InsertPointUnroutedFirst",
                       best: Boolean = false,
                       hotRestart: Boolean = true,
                       nodeSymmetryClass:Option[Int => Int] = None,
                       hotRestartOnNextSymmetryClass:Boolean = false)
  extends BaseInsertPoint(vrp, neighborhoodName, best) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {

    //TODO: il faut itérer sur les points routés en premier, pas l'inverse.
    //et il faut rectifier le facteur de proximité avc la pénalité de non routage dans le cas du InsertPoint!
    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(unroutedNodesToInsert(), startIndice)
      else unroutedNodesToInsert()

    val iterationScheme = nodeSymmetryClass match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }
    cleanRecordedMoves()
    val relevantNeighborsNow = relevantNeighbors()

    val iterationSchemeIterator = iterationScheme.iterator
    while (iterationSchemeIterator.hasNext) {
      insertedPoint = iterationSchemeIterator.next
      assert(!vrp.isRouted(insertedPoint),
        "The search zone should be restricted to unrouted nodes when inserting.")

      val it2 = relevantNeighborsNow(insertedPoint).iterator
      while (it2.hasNext) {
        beforeInsertedPoint = it2.next()
        if (vrp.isRouted(beforeInsertedPoint)) {
          assert(isRecording, "MoveDescription should be recording now")

          encode(beforeInsertedPoint, insertedPoint)
          val newObj = evalObjOnEncodedMove()

          if (evaluateCurrentMoveObjTrueIfStopRequired(newObj)) {
            startIndice = if (hotRestartOnNextSymmetryClass) {
              if (iterationSchemeIterator.hasNext)
                iterationSchemeIterator.next
              else iterationScheme.head
            } else insertedPoint + 1
            return
          }
        }
      }
    }
  }
}


/**
 * OnePoint insert neighborhood htat primarily iterates over insertion point,s and then over poitns that can be iserted.
 * @param insertionPoints the positions where we can insert points
 * @param unroutedNodesToInsert the points to insert, gven an insertion point (use K-nearest here for isntance
 * @param vrp the routing problem
 * @param neighborhoodName the name of the neighborhood
 * @param best best or first move
 * @param hotRestart hot restart on the insertion point
 * @param insertedPointsSymetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @author renaud.delandtsheer@cetic.be
 */
case class InsertPointRoutedFirst(insertionPoints:()=>Iterable[Int],
                                  unroutedNodesToInsert: () => Int => Iterable[Int],
                                  override val vrp: VRP,
                                  neighborhoodName: String = "InsertPointRoutedFirst",
                                  best: Boolean = false,
                                  hotRestart: Boolean = true,
                                  insertedPointsSymetryClass:Option[Int => Int] = None)
  extends BaseInsertPoint(vrp, neighborhoodName, best) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {

    //et il faut rectifier le facteur de proximité avc la pénalité de non routage dans le cas du InsertPoint!
    val iterationSchemeOnInsertionPoint =
      if (hotRestart && !best) HotRestart(insertionPoints(), startIndice)
      else insertionPoints()

    cleanRecordedMoves()
    val unroutedNodesToInsertNow = unroutedNodesToInsert()

    val iterationSchemeOnInsertionPointIterator = iterationSchemeOnInsertionPoint.iterator
    while (iterationSchemeOnInsertionPointIterator.hasNext) {
      beforeInsertedPoint = iterationSchemeOnInsertionPointIterator.next
      assert(vrp.isRouted(insertedPoint), "insertion points shoud all be routed")

      val iteratorOnPointsToInsert = (insertedPointsSymetryClass match {
        case None => unroutedNodesToInsertNow(beforeInsertedPoint)
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(unroutedNodesToInsertNow(beforeInsertedPoint), s)
      }).iterator

      while (iteratorOnPointsToInsert.hasNext) {
        insertedPoint = iteratorOnPointsToInsert.next()
        assert(!vrp.isRouted(insertedPoint))
        assert(isRecording, "MoveDescription should be recording now")

        encode(beforeInsertedPoint, insertedPoint)
        val newObj = evalObjOnEncodedMove()

        if (evaluateCurrentMoveObjTrueIfStopRequired(newObj)) {
          startIndice = beforeInsertedPoint + 1
          return
        }
      }
    }
  }
}



/**
 * base class for point insertion moves
 * @author renaud.delandtsheer@cetic.be
 */
abstract class BaseInsertPoint(vrp: VRP,
                               neighborhoodName: String = "BaseInsertPoint",
                               best: Boolean = false)
  extends EasyRoutingNeighborhood[InsertPointMove](best, vrp, neighborhoodName) {

  var beforeInsertedPoint:Int = 0
  var insertedPoint:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    InsertPointMove(beforeInsertedPoint, insertedPoint, newObj, this, neighborhoodNameToString)

  def encode(beforeInsertedPoint: Int, insertedPoint: Int) {
    assert(!vrp.isRouted(insertedPoint))
    assert(vrp.isRouted(beforeInsertedPoint))
    val s = segmentFromUnrouted(insertedPoint)
    insert(s, beforeInsertedPoint)
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeInsertedPoint the place where to insert an unrouted point.
 * @param insertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param neighborhood the originating neighborhood.
 */
case class InsertPointMove(beforeInsertedPoint: Int,
                           insertedPoint: Int,
                           override val objAfter: Int,
                           override val neighborhood: BaseInsertPoint,
                           override val neighborhoodName: String = "InsertPointMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(beforeInsertedPoint,insertedPoint)

  override def encodeMove() {
    neighborhood.encode(beforeInsertedPoint, insertedPoint)
  }

  override def toString: String =
    neighborhoodName + ":InsertPoint(beforeInsertedPoint = " + beforeInsertedPoint +
      ", insertedPoint = " + insertedPoint + objToString + ")"
}
