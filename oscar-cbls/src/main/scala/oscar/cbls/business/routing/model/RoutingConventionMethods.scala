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

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.{IntSequence, Token}

object RoutingConventionMethods {

  def cachedVehicleReachingPosition(checkpoint:IntSequence,v:Long):((IntSequence,Long) => Long) = {

    val batch = batchVehicleReachingPosition(checkpoint,v:Long)
    def getVehicleReachingPosition(seq:IntSequence,position:Long):Long = {
      if(seq quickEquals checkpoint) batch(position)
      else searchVehicleReachingPosition(position, seq, v)
    }

    getVehicleReachingPosition
  }

  def batchVehicleReachingPosition(seq:IntSequence,v:Long):(Long=>Long) = {
    val vehiclePositionArray:Array[(Long,Long)] =
      Array.tabulate(v)(vehicle => (seq.positionOfAnyOccurrence(vehicle).get, vehicle))

    val vehiclePositionRB = RedBlackTreeMap.makeFromSorted(vehiclePositionArray)

    def findVehicleReachingPosition(position:Long):Long = {
      vehiclePositionRB.biggestLowerOrEqual(position) match {
        case None => v - 1L
        case Some((_, vehicle)) => vehicle
      }
    }
    findVehicleReachingPosition
  }

  @deprecated("use the VehicleLocation method instead","we use stacked checkpoints")
  def searchVehicleReachingPosition(position:Long, seq:IntSequence, v:Long):Long = {
    var upperVehicle = v-1L
    var upperVehiclePosition = seq.positionOfAnyOccurrence(upperVehicle).get

    if(position >= upperVehiclePosition) return upperVehicle

    var lowerVehicle = 0L
    var lowerVehiclePosition = 0L

    assert(seq.positionOfAnyOccurrence(lowerVehicle).get == 0L)
    require(lowerVehiclePosition <= upperVehiclePosition)

    while(lowerVehicle + 1L < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2L
      val midVehiclePosition = seq.positionOfAnyOccurrence(midVehicle).get
      if(midVehiclePosition == position){
        return midVehicle
      }
      if(midVehiclePosition <= position){
        lowerVehicle = midVehicle
        lowerVehiclePosition = midVehiclePosition
      }else{
        upperVehicle = midVehicle
        upperVehiclePosition = midVehiclePosition
      }
    }
    lowerVehicle
  }

  def routingSuccVal2Val(value:Long, seq:IntSequence, v:Long):Long =
    routingSuccPos2Val(seq.positionOfAnyOccurrence(value).get, seq, v)

  def routingSuccPos2Val(position:Long, seq:IntSequence, v:Long):Long = {
    seq.valueAtPosition(position + 1L) match{
      case None => v-1L
      case Some(succToIfNoLoop) => if (succToIfNoLoop < v) succToIfNoLoop-1L else succToIfNoLoop
    }
  }

  def routingPredVal2Val(value:Long, seq:IntSequence, v:Long):Long = {
    if(value < v) {
      //looking for the end node of vehicle value
      if(value == v-1L) {
        //it is the last vehicle
        seq.valueAtPosition(seq.size-1L).get
      }else {
        //there is oe vehicle above
        seq.valueAtPosition(seq.positionOfAnyOccurrence(value+1L).get-1L).get
      }
    }else {
      //simple predecessor
      seq.valueAtPosition(seq.positionOfAnyOccurrence(value).get-1L).get
    }
  }


  def routingPredPos2Val(position:Long, seq:IntSequence, v:Long):Long = {
    routingPredVal2Val(seq.valueAtPosition(position).get, seq, v)
  }

  def isVehicleMoving(vehicle:Long, seq:IntSequence, v:Long):Boolean = {
    routingSuccVal2Val(vehicle, seq:IntSequence, v:Long) != vehicle
  }
}


class CachedPositionOf(maxValue:Long){

  private var tokenOfCurrentCheckpoint:Token = null
  private val checkpointIDOfSavedValue:Array[Token] = Array.fill(maxValue+1L)(null)
  //-1L stands for NONE, -2L is an error
  private val cachedAnyPosition:Array[Long] = Array.fill(maxValue+1L)(-2L)

  def updateToCheckpoint(checkpoint:IntSequence){
    tokenOfCurrentCheckpoint = checkpoint.token
  }
  def positionOfAnyOccurrence(seq:IntSequence,value:Long):Option[Long] = {
    val seqID = seq.token
    if(tokenOfCurrentCheckpoint == seqID){
      if(checkpointIDOfSavedValue(value) == seqID){
        val pos = cachedAnyPosition(value)
        if (pos == -1L) None else Some(pos)
      }else{
        val pos = seq.positionOfAnyOccurrence(value)
        checkpointIDOfSavedValue(value) = tokenOfCurrentCheckpoint
        pos match{
          case None => cachedAnyPosition(value) = -1L
          case Some(x) => cachedAnyPosition(value) = x
        }
        pos
      }
    }else{
      seq.positionOfAnyOccurrence(value)
    }
  }

  def savePos(seq:IntSequence,value:Long,position:Option[Long]){
    if(seq.token == tokenOfCurrentCheckpoint){
      checkpointIDOfSavedValue(value) = tokenOfCurrentCheckpoint
      position match{
        case None => cachedAnyPosition(value) = -1L
        case Some(x) => cachedAnyPosition(value) = x
      }
    }
  }
}
