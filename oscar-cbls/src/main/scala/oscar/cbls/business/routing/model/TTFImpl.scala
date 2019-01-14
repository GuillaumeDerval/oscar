package oscar.cbls.business.routing.model

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

abstract class TravelTimeFunction {
  def getTravelDuration(from: Long, leaveTime: Long, to: Long): Long
  def getBackwardTravelDuration(from: Long, arrivalTime: Long, to: Long): Long

  def getMinMaxTravelDuration(from: Long, to: Long): (Long, Long) =
    (getMinTravelDuration(from, to), getMaxTravelDuration(from, to))

  def getMinTravelDuration(from: Long, to: Long): Long
  def getMaxTravelDuration(from: Long, to: Long): Long
}

/**
  * Stores a square matrix of primitive TTF
  *
  * @param nodeCount the number of nodes to consider; continuous ranges starting at zero
  * @param defaultTTF if we do not specify a TTF for a node, this is the value considered
  * @author renaud.delandtsheer@cetic.be
  */
class TTFMatrix(nodeCount: Long, defaultTTF: PrimitiveTravelTimeFunction) extends TravelTimeFunction {

  private val matrix: Array[Array[PrimitiveTravelTimeFunction]] = Array.fill(nodeCount, nodeCount)(defaultTTF)

  def setTTF(from: Long, to: Long, ttf: PrimitiveTravelTimeFunction) {
    matrix(from)(to) = ttf
  }

  def getTTF(from: Long, to: Long): PrimitiveTravelTimeFunction =
    matrix(from)(to)

  override def getTravelDuration(from: Long, leaveTime: Long, to: Long): Long =
    matrix(from)(to).getTravelDuration(leaveTime)

  override def getBackwardTravelDuration(from: Long, arrivalTime: Long, to: Long): Long =
    matrix(from)(to).getBackwardTravelDuration(arrivalTime)

  override def getMinTravelDuration(from: Long, to: Long): Long =
    matrix(from)(to).getMinTravelDuration

  override def getMaxTravelDuration(from: Long, to: Long): Long =
    matrix(from)(to).getMaxTravelDuration

  override def getMinMaxTravelDuration(from: Long, to: Long): (Long, Long) =
    matrix(from)(to).getMinMaxTravelDuration
}

/**
  * This stores a single TTF of a travel binding two nodes
  * @author renaud.delandtsheer@cetic.be
  */
abstract class PrimitiveTravelTimeFunction {

  /**
    * the duration to perform the travel at leave time "éleaveTime
    *
    * @param leaveTime when the travel begins
    * @return when the travel ends
    */
  def getTravelDuration(leaveTime: Long): Long

  /**
    * the duration of he travel if you plan to arive at the given time
    *
    * @param arrivalTime
    * @return the latest start to begin the travel
    */
  def getBackwardTravelDuration(arrivalTime: Long): Long

  def getMinMaxTravelDuration: (Long, Long) =
    (getMinTravelDuration, getMaxTravelDuration)

  def getMinTravelDuration: Long
  def getMaxTravelDuration: Long

}

/**
  * A TTF that is constant
  * this is similar to using a TTFHistogram with a single slot, but this class is lighter
  * @param travelDuration the duration of the travel
  * @author renaud.delandtsheer@cetic.be
  */
class TTFConst(travelDuration: Long) extends PrimitiveTravelTimeFunction {

  override def toString() = {
    "TTFConst(" + travelDuration + ")"
  }

  override def getTravelDuration(leaveTime: Long): Long = travelDuration

  override def getMinTravelDuration: Long = travelDuration

  override def getMaxTravelDuration: Long = travelDuration

  override def getBackwardTravelDuration(arrivalTime: Long): Long = travelDuration
}

/**
  * represents a TTF using histograms
  * Notice that the representation is modulo, if asked for a time after the overallDuration,
  * it is assumed to start again at position zero in time
  *
  * @param NbSlots the number of slots in the histogram
  * @param overallDuration the duration of the whole TTF
  * @author renaud.delandtsheer@cetic.be
  * THIS IS EXPERIMENTAL
  */
class TTFHistogram(val nbSlots: Long, val overallDuration: Long) extends PrimitiveTravelTimeFunction {
  private val slotDuration: Long = overallDuration / nbSlots

  private val slots: Array[Long] = Array.fill(nbSlots)(0L)
  private var nimMaxAccurate = false
  private var min: Long = 0L
  private var max: Long = 0L

  override def toString() = {
    "TTFHistogram(nbSlots " + nbSlots + ", " + { var strSlots = ""; for (slot <- slots) { strSlots += slot + "; " }; strSlots } + ")"
  }

  def setTravelDurationAtSlot(slotNumber: Long, duration: Long) {
    slots(slotNumber) = duration
    nimMaxAccurate = false
  }
  def getTravelDurationAtSlot(slotNumber: Long): Long = slots(rectifySlot(slotNumber))

  private def updateMinMax() {
    if (!nimMaxAccurate) {
      nimMaxAccurate = true
      min = Long.MaxValue
      max = Long.MinValue
      for (v <- slots) {
        if (v < min) min = v
        if (max < v) max = v
      }
    }
  }

  private def rectifySlot(slotNr: Long): Long = {
    var tmp: Long = slotNr % nbSlots
    if (tmp < 0L) tmp += nbSlots
    tmp
  }

  override def getTravelDuration(leaveTime: Long): Long =
    getTravelDurationAtSlot(leaveTime / slotDuration)

  override def getMinTravelDuration: Long = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Long = {
    updateMinMax()
    max
  }

  override def getBackwardTravelDuration(arrivalTime: Long): Long = {
    if (nbSlots == 1L) {
      slots(0L)
    }
    var maxslot: Long = arrivalTime / slotDuration;
    var minslot: Long = 0L
    while (minslot * slotDuration + getTravelDurationAtSlot(minslot) >= arrivalTime) {
      minslot -= nbSlots
    }

    while (true) {
      if (minslot == maxslot) {
        return slots(rectifySlot(minslot))
      } else if (minslot + 1L == maxslot) {
        if (maxslot * slotDuration + getTravelDurationAtSlot(maxslot) <= arrivalTime) {
          return getTravelDurationAtSlot(maxslot)
        } else {
          return getTravelDurationAtSlot(minslot)
        }
      }
      val medslot = (minslot + maxslot) / 2L
      val medslotstart = medslot * slotDuration

      if (medslotstart + getTravelDurationAtSlot(medslot) <= arrivalTime) {
        minslot = medslot
      }
      if (medslotstart + slotDuration + getTravelDurationAtSlot(medslot) >= arrivalTime) {
        maxslot = medslot
      }
    }
    // Default return value
    0L
  }
}

/**
  * Represents a TTF as a piecewise linear function
  *
  * Notice that the representation is modulo, if asked for a time after the last point,
  * it is assumed to start again at position zero in time,
  * so that linear interpolation might happen between the last point and the first point, shifted by overallDuration
  *
  * @param NbPoints the number of points to consider
  * @param overallDuration the duration to consider
  * @author renaud.delandtsheer@cetic.be
  */
class TTFSegments(val NbPoints: Long, val overallDuration: Long) extends PrimitiveTravelTimeFunction {

  private val pointX: Array[Long] = Array.fill(NbPoints)(0L)
  private val pointY: Array[Long] = Array.fill(NbPoints)(0L)

  private var nimMaxAccurate = false
  private var min: Long = 0L
  private var max: Long = 0L

  /**throws an error if the X is smaller than the predecessor's X, or if the slope is too steep*/
  def setPoint(pointNr: Long, pointX: Long, pointY: Long) {
    this.pointX(pointNr) = pointX
    this.pointY(pointNr) = pointY
    nimMaxAccurate = false
    if (pointNr != 0L) {

      val firstId = pointNr - 1L
      val secondId = pointNr

      val intervalX = this.pointX(secondId) - this.pointX(firstId)
      if (intervalX <= 0L) throw new Error("TTF segments are going backward in time")

      val intervalY = this.pointY(secondId) - this.pointY(firstId)
      if (intervalX < -intervalY) throw new Error("slope is too steep")
    }
  }

  def getPointX(pointNr: Long): Long = getPoint(pointNr)._1
  def getPointY(pointNr: Long): Long = getPoint(pointNr)._2

  def getPoint(pointNr: Long): (Long, Long) = {
    var rectifiedPoint: Long = pointNr % NbPoints
    var shifting: Long = math.floor(pointNr / NbPoints).toInt * overallDuration
    while (rectifiedPoint < 0L) {
      rectifiedPoint += NbPoints
      shifting -= overallDuration
    }

    (pointX(rectifiedPoint) + shifting, pointY(rectifiedPoint))
  }

  private def updateMinMax() {
    if (!nimMaxAccurate) {
      nimMaxAccurate = true
      min = Long.MaxValue
      max = Long.MinValue
      for (v <- pointY) {
        if (v < min) min = v
        if (max < v) max = v
      }
    }
  }

  /**
    *
    * @param x is a point in time
    * @return a point number
    */
  private def findLastPointBefore(x: Long): Long = {
    var up: Long = NbPoints - 1L
    while (getPointX(up) < x) up = up + NbPoints
    var down: Long = -1L
    while (getPointX(down) > x) down = down - NbPoints

    while (down + 1L < up) {
      val mid: Long = (up + down) / 2L
      if (getPointX(mid) == x) {
        return mid
      } else if (getPointX(mid) < x) {
        down = mid
      } else {
        up = mid
      }
    }
    if (getPointX(up) <= x) up
    else down
  }

  override def getTravelDuration(leaveTime: Long): Long = {
    val pointBefore = findLastPointBefore(leaveTime)
    val pointAfter = pointBefore + 1L

    linearInterpol(leaveTime,
      getPointX(pointBefore), getPointY(pointBefore),
      getPointX(pointAfter), getPointY(pointAfter)).toInt
  }

  @inline
  private def linearInterpol(X: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    ((X - X1) * (Y2 - Y1)) / (X2 - X1) + Y1
  }

  override def getMinTravelDuration: Long = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Long = {
    updateMinMax()
    max
  }

  def findLastPointBeforeLeave(arrivalTime: Long): Long = {
    var up: Long = NbPoints - 1L
    while (getPointX(up) + getPointY(up) < arrivalTime) up = up + NbPoints
    var down: Long = -1L
    while (getPointX(down) + getPointX(down) > arrivalTime - overallDuration) down = down - NbPoints

    while (down + 1L < up) {
      val mid: Long = (up + down) / 2L
      if (getPointX(mid) + getPointY(mid) == arrivalTime) {
        return mid
      } else if (getPointX(mid) + getPointY(mid) < arrivalTime) {
        down = mid
      } else {
        up = mid
      }
    }
    if (getPointX(up) + getPointY(up) <= arrivalTime) up
    else down
  }

  override def getBackwardTravelDuration(arrivalTime: Long): Long = {
    var pointBefore = findLastPointBeforeLeave(arrivalTime)
    while (getPointX(pointBefore + 1L) + getPointY(pointBefore + 1L) <= arrivalTime) {
      pointBefore += 1L
    }

    assert(getPointX(pointBefore) + getPointY(pointBefore) <= arrivalTime)
    assert(arrivalTime <= getPointX(pointBefore + 1L) + getPointY(pointBefore + 1L))

    linearInterpolBackward(arrivalTime,
      getPointX(pointBefore), getPointY(pointBefore),
      getPointX(pointBefore + 1L), getPointY(pointBefore + 1L)).toInt
  }

  @inline
  private def linearInterpolBackward(Y: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    if (Y1 == Y2) return Y1
    val p = (X1 - X2) / (Y1 - Y2)
    ((Y + p * Y1 - X1) / (p + 1.0)).toFloat
  }

  override def toString = ("TTFSegments(NbPoints: " + NbPoints + " overallDuration: " + overallDuration + " points: ["
    + ((0L until NbPoints) map (i => "(" + pointX(i) + ";" + pointY(i) + ")") mkString ",") + "])")

}

object TTFTest extends App {

  val t = new TTFSegments(7L, 24L * 60L)
  t.setPoint(0L, 60L * 3L, 10L)
  t.setPoint(1L, 60L * 6L, 20L)
  t.setPoint(2L, 60L * 9L, 30L)
  t.setPoint(3L, 60L * 12L, 20L)
  t.setPoint(4L, 60L * 15L, 30L)
  t.setPoint(5L, 60L * 18L, 30L)
  t.setPoint(6L, 60L * 21L, 10L)

  println(t)

  println("leave\tarrive")
  for (i <- 0L to (24L * 60L) by 30L) {
    println(i + "\t" + t.getTravelDuration(i) + "\t" + t.getBackwardTravelDuration(t.getTravelDuration(i) + i))
  }
  println("min: " + t.getMinTravelDuration + " max: " + t.getMaxTravelDuration)
}
object TTFHistoTest extends App{
  val t = new TTFHistogram(7L, 24L*60L)

  t.setTravelDurationAtSlot(0L, 2L*60L)
  t.setTravelDurationAtSlot(1L, 2L*60L)
  t.setTravelDurationAtSlot(2L, 3L*60L)
  t.setTravelDurationAtSlot(3L, 6L*60L)
  t.setTravelDurationAtSlot(4L, 5L*60L)
  t.setTravelDurationAtSlot(5L, 4L*60L)
  t.setTravelDurationAtSlot(6L, 3L*60L)

  println(t)
  println("leave\tarrive")
  for(i <- 0L to (24L * 60L) by 30L){
    println(i + "\t" + t.getTravelDuration(i) + "\t" + t.getBackwardTravelDuration(t.getTravelDuration(i) + i))
  }
}
