package oscar.cbls.business.routing.model

import oscar.cbls.algo.seq.IntSequence

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

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
object VehicleLocation{
  def apply(v:Long,nodeToPosition:Long=>Long): ConcreteVehicleLocation = new ConcreteVehicleLocation(Array.tabulate(v)(nodeToPosition))
  def apply(startPositionOfVehicleThatIWillNeverModify:Array[Long]) = new ConcreteVehicleLocation(startPositionOfVehicleThatIWillNeverModify)
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
abstract class VehicleLocation(val v : Long, val level:Long){

  def checkOnSequence(s:IntSequence){
    for(vehicle <- 0L until v){
      require(s.positionOfAnyOccurrence(vehicle).get == startPosOfVehicle(vehicle),vehicle)
    }
  }

  def regularize:ConcreteVehicleLocation = VehicleLocation(v,startPosOfVehicle)

  /**
   *
   * @param oldPosToNewPos a description of the move through a function that maps old position to new position for all values in the sequence.
   * @param maxStackLevel is the maximal level of stack before a regularization automatically takes place. by default it is set to 2L*v
   * @return a vehicle location reflecting the start position of vehicle after the move is performed
   */
  def push(oldPosToNewPos:(Long)=> Option[Long], maxStackLevel:Long = 2L*v): VehicleLocation = {
    val tmp = new StackedVehicleLocation(oldPosToNewPos,this)
    if(tmp.level > maxStackLevel) tmp.regularize
    else tmp
  }

  def startPosOfVehicle(vehicle:Long):Long

  def vehicleReachingPosition(position:Long): Long ={
    var upperVehicle:Long = v -1L
    var upperVehiclePosition = startPosOfVehicle(upperVehicle)
    if(position>=upperVehiclePosition) return upperVehicle
    var lowerVehicle = 0L
    var lowerVehiclePosition = 0L
    while(lowerVehicle + 1L < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2L
      val midVehiclePosition = startPosOfVehicle(midVehicle)
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
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
class ConcreteVehicleLocation(private val startPositionOfVehicle:Array[Long]) extends VehicleLocation(startPositionOfVehicle.length, 0L){

  override def regularize: ConcreteVehicleLocation = this

  def startPosOfVehicle(vehicle: Long): Long = startPositionOfVehicle(vehicle)

  override def toString: String = "ConcreteVehicleLocation(" + startPositionOfVehicle.mkString(",") + ")"
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
class StackedVehicleLocation(val oldPosToNewPos:Long=> Option[Long], val prev: VehicleLocation) extends VehicleLocation(prev.v, prev.level + 1L){

  def startPosOfVehicle(vehicle: Long): Long = oldPosToNewPos(prev.startPosOfVehicle(vehicle)).get

  override def toString: String = "StackedVehicleLocation([" + (0L until v).map(startPosOfVehicle).mkString(",") + "] depth:" + level + " prev:" + prev + ")"
}
