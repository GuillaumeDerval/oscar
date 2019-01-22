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

package oscar.cbls.lib.search.neighborhoods

import oscar.cbls._
import oscar.cbls.algo.lazyIt.LazyMap
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.search.{Move, EasyNeighborhood}


/**
 * flips a section of the array, only contiguous zones are searched
 * the search is organized efficiently by widening a flip zone around a central pint, that is moved in the outer loop
 * this allows us to reduce the number of updates between successive neighbors, possibly reducing run time by a factor O5),
 * also depending on the model impacted by vars.
 * also, we consider flip with an orr or even number of involved variables
 *
 * for each center of flip zone, taken as all flippeable positions in the array, sorted by decreasing maximal flip size
 *    for each width of the fliped zone, by increasing order, and interrupted whenever a non-flippeable position is reached
 *      test flipping
 */
//TODO add the possibility to specify positions that must be in the limit of the flip?
//TODO: hotRestart
case class WideningFlipNeighborhood(vars:Array[CBLSIntVar],
                                    name:String = "WideningFlipNeighborhood",
                                    allowedPositions:()=>Iterable[Long] = null,
                                    maxFlipSize:Long = Long.MaxValue,
                                    minFlipSize:Long = 2L,
                                    exploreLargerOpportunitiesFirst:Boolean = true,
                                    best:Boolean = false,
                                    hotRestart:Boolean = true)
  extends EasyNeighborhood[FlipMove](best,name) {
  require(minFlipSize > 1L, "minFlipSize should be >1L")

  val varSize = vars.length
  val lastPosition = varSize - 1L

  val allAllowed = if (allowedPositions == null) {
    Array.fill(varSize)(true)
  } else null

  var currentFromPosition = 0L
  var currentToPosition = 0L

  def computeDistanceFromFirstUnauthorizedPosition(isAllowed: Array[Boolean]) = {
    val distanceFromFirstUnauthorizedPosition: Array[Long] = Array.fill(varSize)(0L)
    var lastUnauthorizedPosition = -1L
    var currentPOsition = 0L
    while (currentPOsition <= lastPosition) {
      if (isAllowed(currentPOsition)) {
        distanceFromFirstUnauthorizedPosition(currentPOsition) = currentPOsition - lastUnauthorizedPosition
      } else {
        lastUnauthorizedPosition = currentPOsition
        distanceFromFirstUnauthorizedPosition(currentPOsition) = 0L
      }
      currentPOsition += 1L
    }
    distanceFromFirstUnauthorizedPosition
  }

  def computeDistanceToFirstUnauthorizedPosition(isAllowed: Array[Boolean]) = {
    val distanceToFirstUnauthorizedPosition = Array.fill(varSize)(0L)
    var currentPosition = lastPosition
    var lastUnahtorizedPOsition = varSize
    while (currentPosition > 0L) {
      if (isAllowed(currentPosition)) {
        distanceToFirstUnauthorizedPosition(currentPosition) = lastUnahtorizedPOsition - currentPosition
      } else {
        lastUnahtorizedPOsition = currentPosition
        distanceToFirstUnauthorizedPosition(currentPosition) = 0L
      }
      currentPosition -= 1L
    }
    distanceToFirstUnauthorizedPosition
  }

  override def exploreNeighborhood(): Unit = {
    //build allowed array

    val isAllowed = if (allowedPositions != null) {
      val tmp = Array.fill(varSize)(false)
      for (p <- allowedPositions()) tmp(p) = true
      tmp
    } else {
      allAllowed
    }

    val flipCenterIterable:Iterable[(Long,Long,Long)] =
      if(exploreLargerOpportunitiesFirst) computeFlipCentersLargestFirst(isAllowed)
      else computeFlipCentersLeftFirst(isAllowed)

    val flipCenterITerator = flipCenterIterable.iterator
    while(flipCenterITerator.hasNext){
      val (fromPosition, toPosition, maxReacheableFlipSize) = flipCenterITerator.next()
      if (exploreAndflipToMinimalFlipSize(fromPosition, toPosition, isAllowed)) {
        return
      }
    }
  }

  def computeFlipCentersCanonicalHotRestart(isAllowed:Array[Boolean]):Iterable[(Long,Long,Long)] = {
    throw new Error("not implemented") //TODO
  }

  def computeFlipCenters(isAllowed:Array[Boolean]):Iterable[(Long,Long,Long)] = {
    //compute distance to and from unauthorized positions
    val distanceFromFirstUnauthorizedPosition = computeDistanceFromFirstUnauthorizedPosition(isAllowed)
    val distanceToFirstUnauthorizedPosition = computeDistanceToFirstUnauthorizedPosition(isAllowed)

    //will contain all the flip centers
    var flipCenters: List[(Long, Long, Long)] = List.empty

    val thresholdForNeareseFlipEven = 1L max (minFlipSize / 2L)
    //compute allowed centres for even flips (pairs)
    var currentPosition = 0L
    while (currentPosition <= lastPosition - 1L) {
      val initialFromPosition = currentPosition
      val initialToPosition = currentPosition + 1L
      if ((distanceFromFirstUnauthorizedPosition(initialFromPosition) >= thresholdForNeareseFlipEven)
        && (distanceToFirstUnauthorizedPosition(initialToPosition) >= thresholdForNeareseFlipEven)) {
        val centre = (initialFromPosition, initialToPosition, distanceFromFirstUnauthorizedPosition(initialFromPosition) min distanceToFirstUnauthorizedPosition(initialToPosition))
        flipCenters = centre :: flipCenters
      }
      currentPosition += 1L
    }

    //compute allowed centres for odd (impair) flips
    currentPosition = 0L
    val thresholdForNeareseFlipOdd = 1L max ((minFlipSize - 1L) / 2L)

    while (currentPosition <= lastPosition - 2L) {
      val initialFromPosition = currentPosition
      val initialToPosition = currentPosition + 2L
      if ((distanceFromFirstUnauthorizedPosition(initialFromPosition) >= thresholdForNeareseFlipOdd)
        && (distanceToFirstUnauthorizedPosition(initialToPosition) >= thresholdForNeareseFlipOdd)) {
        val centre = (initialFromPosition, initialToPosition, 1L + (distanceFromFirstUnauthorizedPosition(initialFromPosition) min distanceToFirstUnauthorizedPosition(initialToPosition)))
        flipCenters = centre :: flipCenters
      }
      currentPosition += 1L
    }
    flipCenters
  }

  def computeFlipCentersLargestFirst(isAllowed:Array[Boolean]):Iterable[(Long,Long,Long)] = {
    val flipCenters = computeFlipCenters(isAllowed)
    val allCentersInarray = flipCenters.toArray
    val referenceArray = Array.tabulate(allCentersInarray.length)(i => intToLong(i))
    new LazyMap(KSmallest.lazySort(referenceArray,id => -allCentersInarray(id)._3),id => allCentersInarray(id))
  }

  def computeFlipCentersLeftFirst(isAllowed:Array[Boolean]):Iterable[(Long,Long,Long)] = {
    val flipCenters = computeFlipCenters(isAllowed)
    val allCentersInArray = flipCenters.toArray
    val flipCenterCount = allCentersInArray.length
    val referenceArray = Array.tabulate(allCentersInArray.length)(i => intToLong(i))
    new LazyMap(KSmallest.lazySort(referenceArray,id => -(allCentersInArray(id)._1 * flipCenterCount + allCentersInArray(id)._2)),id => allCentersInArray(id))
  }


  /**
   *  fromPosition and toPosition are different
   *  these positions have not been swapped yet
   *  the positions in-between are already flipped
   *
   * @param fromPosition
   * @param toPosition
   * @param isAllowed
   * @return true if search must be stopped, false otherwise; the segment will already be flipped back on return (to have a  terminal recursion)
   * */
  def exploreAndflipToMinimalFlipSize(fromPosition:Long,toPosition:Long, isAllowed:Array[Boolean]):Boolean = {
    if(fromPosition + minFlipSize <= toPosition){
      //we can start
      explore(fromPosition,toPosition, isAllowed)
    }else if(fromPosition >= 0L && isAllowed(fromPosition) && toPosition <= lastPosition && isAllowed(toPosition) && (toPosition - fromPosition) <= maxFlipSize){
      //still to incrase the skipSize
      vars(fromPosition) :=: vars(toPosition)

      exploreAndflipToMinimalFlipSize(fromPosition-1L,toPosition+1L,isAllowed)
    }else{
      //reached some stop condition (should not happen if centers are properly chosen)
      FlipMove.doFlip(fromPosition+1L,toPosition-1L,vars)
      false
    }
  }

  /**
   *  fromPosition and toPosition are different
   *  these positions have not been swapped yet
   *  the positions in-between are already flipped
   * @return true if search must be stopped, false otherwise; the segment will already be flipped back on return (to have a  terminal recursion)
   */
  def explore(fromPosition:Long,toPosition:Long, isAllowed:Array[Boolean]): Boolean = {
    if(fromPosition >= 0L && isAllowed(fromPosition) && toPosition <= lastPosition && isAllowed(toPosition) && (toPosition - fromPosition) <= maxFlipSize){
      vars(fromPosition) :=: vars(toPosition)
      currentFromPosition = fromPosition
      currentToPosition = toPosition
      if (evaluateCurrentMoveObjTrueIfStopRequired(obj.value)) {
        //flip back
        FlipMove.doFlip(fromPosition,toPosition,vars)
        return true
      } else {
        explore(fromPosition-1L,toPosition+1L,isAllowed)
      }
    } else {
      //exploration is over, no interrupting move found
      FlipMove.doFlip(fromPosition+1L,toPosition-1L,vars)
      return false
    }
  }

  override def instantiateCurrentMove(newObj: Long): FlipMove = FlipMove(currentFromPosition,currentToPosition,vars,newObj,name)
}


object FlipMove{
  def doFlip(fromPosition:Long,toPosition:Long,variables:Array[CBLSIntVar]){
    if(fromPosition < toPosition){
      variables(fromPosition) :=: variables(toPosition)
      doFlip(fromPosition+1L,toPosition-1L,variables)
    }
  }
}

case class FlipMove(fromPosition:Long,toPosition:Long,variables:Array[CBLSIntVar], override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName) {

  require(fromPosition < toPosition)

  override def commit(): Unit = {
    FlipMove.doFlip(fromPosition,toPosition,variables)
  }

  override def toString: String = {
    neighborhoodNameToString + "FlipMove(fromPosition:" + fromPosition + " toPosition:" + toPosition + " size:" + (toPosition - fromPosition) +  objToString + ")"
  }
}
