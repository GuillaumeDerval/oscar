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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 *
 *     Refactored with respect to the new architecture by Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart

/**
 * Removes three edges of routes, and rebuilds routes from the segments.
 * Finds 3 candidate points for a 3-opt move, and then
 * chooses on-the-fly between simple 3-opt move and reverse 3-opt move.
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class ThreeOpt(potentialInsertionPoints:()=>Iterable[Int],
                    relevantNeighbors:()=>Int=>Iterable[Int],
                    vrp: VRP with PositionInRouteAndRouteNr,
                    neighborhoodName:String = null,
                    best:Boolean = false,
                    hotRestart:Boolean = true,
                    KKIterationScheme:Boolean = true) extends EasyRoutingNeighborhood(best,vrp,neighborhoodName) {

  val REVERSE = true // this is a constant used for readability

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood() {
    if (KKIterationScheme) {
      exploreNeighborhoodKK()
    } else {
      exploreNeighborhoodRouteExtension()
    }
  }

  def exploreNeighborhoodKK(): Unit ={
    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(potentialInsertionPoints(), startIndice)
      else potentialInsertionPoints()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    for (insertionPoint <- iterationSchemeOnZone
         if vrp.isRouted(insertionPoint)) {

      require(isRecording, "VRP should be recording")

      val otherNodes:List[List[Int]] = relevantNeighborsNow(insertionPoint)
        .filter((neighbor:Int) => vrp.isRouted(neighbor) && neighbor != insertionPoint)
        .groupBy(vrp.routeNr(_).value)
        .toList
        .map(_._2.toList)

      for(nodeList <- otherNodes){
        for((a,b) <- makeAllUnsortedPairs(nodeList)){
          val (first,second) = if(vrp.positionInRoute(a).value < vrp.positionInRoute(b).value) (a,b) else (b,a)

          if(!vrp.isBetween(insertionPoint, first, second)
            && !(vrp.next(insertionPoint).value == first)){

            if(chooseBest3Opt(first, vrp.next(first).value, second, insertionPoint)){
              startIndice = insertionPoint + 1
              return
            }
          }
        }
      }
    }
  }

  /**
   * @param l a list
   * @return a list of all pairs of element made from the elements in l
   */
  private def makeAllUnsortedPairs(l:List[Int]):List[(Int,Int)] = {
    def makeAllUnsortedPairsWithHead(head:Int, tail:List[Int], toAppend:List[(Int,Int)]):List[(Int,Int)] = {
      tail match{
        case other :: newTail => makeAllUnsortedPairsWithHead(head, newTail, (head,other) :: toAppend)
        case Nil => toAppend
      }
    }

    l match{
      case Nil => List.empty
      case head :: tail => makeAllUnsortedPairsWithHead(head,tail,makeAllUnsortedPairs(tail))
    }
  }

  def exploreNeighborhoodRouteExtension(){
    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(potentialInsertionPoints(), startIndice)
      else potentialInsertionPoints()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    // The insertion point is picked from the primaryNodeIterator.
    for (insertionPoint <- iterationSchemeOnZone
         if vrp.isRouted(insertionPoint)) {

      /*
       * The segment predecessor point (beforeStart) is picked from the insertion point
       * neighbors. It must not be the same as the insertion point, because the move would
       * not change the route.
       * 
       * The segment start point is the successor of beforeStart, it must not be a depot, neither
       * the same point.
       */
      for (beforeStart <- relevantNeighborsNow(insertionPoint)) {
        if (beforeStart != insertionPoint) {
          val segStartPoint = vrp.next(beforeStart).value
          if (segStartPoint != beforeStart && !vrp.isADepot(segStartPoint)) {

            /**
             * The segment end point is picked from the next nodes of its start point route.
             */
            var segEndPoint = vrp.next(segStartPoint).value
            var afterEnd = vrp.next(segEndPoint).value
            while (segEndPoint != beforeStart && !vrp.isADepot(segEndPoint)) {
              // isBetween(i, b, a) checks i:[b, a[  (and i is in the same route as b and a)
              // we can't test with afterEnd instead of segEndPoint because it would not work
              // when afterEnd = beforeStart
              if (!vrp.isBetween(insertionPoint, beforeStart, segEndPoint)
                && insertionPoint != segEndPoint) {

                if (chooseBest3Opt(beforeStart, segStartPoint, segEndPoint, insertionPoint)){
                  startIndice = insertionPoint + 1
                  return
                }
              }

              segEndPoint = afterEnd
              afterEnd = vrp.next(segEndPoint).value
            }
          }
        }
      }
    }
  }

  /**
   * returns true if search can be stopped
   */
  def chooseBest3Opt(beforeStart: Int, segStartPoint: Int, segEndPoint: Int,
                     insertionPoint: Int): Boolean = {

    /**
     * FIRST, we do a simple 3-opt move,
     * with UNDO DEACTIVATED,
     * and we save if such a move is improving.
     */
    encodeMove(beforeStart, segEndPoint, insertionPoint, !REVERSE)
    commit(false)
    val objAfterFirstMove = obj()

    /**
     * SECOND, we reverse the moved segment, in place,
     * with UNDO ACTIVATED, so that we can go back to the previous move if necessary
     */
    reverseSegmentInPlace(insertionPoint, segEndPoint) // REVERSE
    commit(false)
    val objAfterSecondMove = obj()

    val FirstMoveIsBestMove = objAfterFirstMove < objAfterSecondMove
    val bestObjAfter = if(FirstMoveIsBestMove) objAfterFirstMove else objAfterSecondMove

    //put everything back to place, since we three-opted and reversed, the rollback performs the reverse
    encodeMove(insertionPoint, segStartPoint, beforeStart, REVERSE)
    commit(false)

    (moveRequested(bestObjAfter)
      && submitFoundMove(ThreeOptMove(beforeStart, segEndPoint, insertionPoint,
      !FirstMoveIsBestMove, bestObjAfter, this, neighborhoodNameToString)))
  }

  //this resets the internal state of the Neighborhood
  override def reset(){
    startIndice = 0
  }

  /**
   * Do a 3-opt move which is : cuts a segment and inserts it (reversed if necessary)
   * in another place.
   *
   * beforeStart->[...->segEndPoint]
   * becomes
   * insertionPoint->[...->segEndPoint]
   * or
   * insertionPoint->[segEndPoint->...]
   */
  def encodeMove(beforeStart: Int, segEndPoint: Int, insertionPoint: Int,
                 reverseSegment: Boolean) {
    cleanRecordedMoves()
    var seg = cut(beforeStart, segEndPoint)
    if (reverseSegment) {
      seg = reverse(seg)
    }
    insert(seg, insertionPoint)
  }
}


/**
 * Models a three-opt-move operator of a given VRP problem.
 * @param beforeStart the predecessor of the moved segment.
 * @param segEndPoint the end of the moved segment.
 * @param insertionPoint the place where to insert the moved segment.
 * @param reverseSegment true if the segment will be inverted before being inserted
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class ThreeOptMove(beforeStart: Int,
                        segEndPoint: Int,
                        insertionPoint: Int,
                        reverseSegment: Boolean,
                        override val objAfter: Int,
                        override val neighborhood:ThreeOpt,
                        override val neighborhoodName:String = null)
  extends VRPMove(objAfter, neighborhood, neighborhoodName) {

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(beforeStart, segEndPoint, insertionPoint, reverseSegment)
  }

  override def toString: String =
    ("TreeOpt(point before segment start = " + beforeStart
      + ", segment end point = " + segEndPoint
      + ", insertion point = " + insertionPoint
      + ", reverse segment = " + reverseSegment + ")")
}
