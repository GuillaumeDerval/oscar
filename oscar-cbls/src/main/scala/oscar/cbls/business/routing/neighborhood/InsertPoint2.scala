package oscar.cbls.business.routing.neighborhood

/*******************************************************************************
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
  ******************************************************************************/


import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{LoopBehavior, EasyNeighborhoodMultilevel, EasyNeighborhood}

/**
 * base class for point insertion moves
 * @author renaud.delandtsheer@cetic.be
 */
abstract class InsertPoint2(vrp: VRP,
                           neighborhoodName: String)
  extends EasyNeighborhoodMultilevel[InsertPointMove2](neighborhoodName){

  val v = vrp.v
  val seq = vrp.routes

  var insertAtPositionForInstantiation:Int = -1
  var insertedPointForInstantiation:Int = -2

  override def instantiateCurrentMove(newObj: Int) =
    InsertPointMove2(insertedPointForInstantiation, insertAtPositionForInstantiation, newObj, this, vrp, neighborhoodNameToString)

  def doMove(insertedPoint: Int, insertAtPosition:Int) {
    seq.insertAtPosition(insertedPoint, insertAtPosition)
  }
}

case class InsertPointMove2(insertedPoint: Int,
                           insertAtPosition: Int,
                           override val objAfter: Int,
                           override val neighborhood: InsertPoint2,
                           vrp:VRP,
                           override val neighborhoodName: String = "InsertPointMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  //TODO
  override def impactedPoints: List[Int] = List(insertedPoint)

  override def commit() {
    neighborhood.doMove(insertedPoint, insertAtPosition)
  }

  override def toString: String =
    neighborhoodName + ":InsertPoint(insertedPoint:" + insertedPoint +
      " insertAtPosition:" + insertAtPosition + objToString + ")"
}


/**
 * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
 * where u is the numberof unrouted points, and n is the number of routed points
 * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
 * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
 * @param relevantPredecessor a function that, for each unrouted node gives a list of routed node
 *                          such that it is relevant to insert the unrouted node after this routed node
 * @param vrp the routing problem
 * @param neighborhoodName the name of this neighborhood
 * @param selectNodeBehavior how should it iterate on nodes to insert?
 * @param selectInsertionPointBehavior how should it iterate on position for insertion?
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
case class InsertPointUnroutedFirst2(unroutedNodesToInsert: () => Iterable[Int],
                                    relevantPredecessor: () => Int => Iterable[Int],
                                    vrp: VRP,
                                    neighborhoodName: String = "InsertPointUnroutedFirst",
                                    hotRestart: Boolean = true,
                                    selectNodeBehavior:LoopBehavior,
                                    selectInsertionPointBehavior:LoopBehavior,
                                    nodeSymmetryClass:Option[Int => Int] = None,
                                    hotRestartOnNextSymmetryClass:Boolean = false)
  extends InsertPoint2(vrp: VRP,neighborhoodName){

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(unroutedNodesToInsert(), startIndice)
      else unroutedNodesToInsert()

    val iterationScheme = nodeSymmetryClass match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }

    val relevantNeighborsNow = relevantPredecessor()

    val (nodeToInsertIterable,notifyFound1) = selectNodeBehavior.toIterable(iterationScheme)
    val nodeToInsertIterator = nodeToInsertIterable.iterator

    while (nodeToInsertIterator.hasNext) {
      insertedPointForInstantiation = nodeToInsertIterator.next()
      assert(!vrp.isRouted(insertedPointForInstantiation),
        "The search zone should be restricted to unrouted nodes when inserting.")

      val (positionToInsertIterable,notifyFound2) =
        selectInsertionPointBehavior.toIterable(relevantNeighborsNow(insertedPointForInstantiation))

      val it2 = positionToInsertIterable.iterator
      while (it2.hasNext) {
        val pointWhereToInsertAfter = it2.next()
        seqValue.positionOfAnyOccurrence(pointWhereToInsertAfter) match{
          case None => //not routed?!
          case Some(position) =>
            insertAtPositionForInstantiation = position + 1

            doMove(insertedPointForInstantiation, insertAtPositionForInstantiation)

            if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
              notifyFound1()
              notifyFound2()
            }
        }
      }
    }


    seq.releaseTopCheckpoint()

    //hotRestart stuff for next call
    startIndice = if (hotRestartOnNextSymmetryClass) {
      if (nodeToInsertIterator.hasNext)
        nodeToInsertIterator.next()
      else iterationScheme.head
    } else{insertedPointForInstantiation + 1}

  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}
