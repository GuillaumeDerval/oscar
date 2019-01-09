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
  * @author Gustav Björdal
  */
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.numeric.{MinusOffsetPos, Prod2, Sum}

/*import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.numeric.{MinusOffsetPos, Prod2, Sum}
*/

/**
  * Constrains the a resource usage to be lower than some limit.
  * The violation is the sum of the overflow at each time step:
  *  sum(t in 0 .. horizon)(max(0,limit - usage[t]))
  *
  * @param start the start time of tasks
  * @param duration the duration of tasks
  * @param amount the amount that tasks use of the resource
  * @param limit the resource limit
  * @author gustav.bjordal@it.uu.se
  */
case class CumulativePrototype(start: Array[IntValue], duration: Array[IntValue], amount:Array[IntValue], limit:IntValue) extends Invariant with Constraint with IntNotificationTarget{



  for (v <- start.indices) registerStaticAndDynamicDependency(start(v), v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v), v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v), v)
  registerStaticAndDynamicDependency(limit)

  registerConstrainedVariable(limit)
  registerConstrainedVariables(start)
  registerConstrainedVariables(duration)
  registerConstrainedVariables(amount)

  finishInitialization()
  private val horizon:(Int,Int) = (start.map(_.min).min,start.map(_.max).max + duration.map(_.max).max)
  private val profile = new CumulativeProfile(model,start.length,horizon,amount.map(_.max).sum,limit)
  for(v <- profile.blocks){
    v.height.setDefiningInvariant(this)
    v.width.setDefiningInvariant(this)
  }
  private val Violation = Sum(profile.blocks.map(_.blockViolation))

  for(v <- start.indices){
    profile.addToProfile(start(v).value, duration(v).value, amount(v).value)
  }

  val variableViolation: Array[CBLSIntVar] = start.map(_ => CBLSIntVar(model, 0, 0 to amount.map(_.max).sum))
  for(v <- variableViolation) v.setDefiningInvariant(this)
  updateVarViolation(horizon._1, horizon._2)

  override def violation: Sum = Violation
  override def violation(v: Value): IntValue = {
    if(start.indexOf(v.asInstanceOf[IntValue]) != -1){
      variableViolation(start.indexOf(v.asInstanceOf[IntValue]))
    }else if(v == limit){
      Violation
    }else{
      0
    }
  }

  def updateVarViolation(s:Int, d:Int): Unit = {
    //Verify that these bounds are correct
    for(i <- start.indices
        if(start(i).value >= s && start(i).value < s+d) ||
          (start(i).value < s && start(i).value+ duration(i).value-1 > s+d) ||
          (start(i).value+duration(i).value-1 >= s && start(i).value + duration(i).value-1 < s+d)){
      variableViolation(i) := profile.getViolation(start(i).value, duration(i).value)
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    if(index == -1){
      updateVarViolation(horizon._1,horizon._2)
    }else if (start(index) == v && amount(index).value >0) {
      //start
      val d = duration(index).value
      val a = amount(index).value
      if(Math.abs(OldVal-NewVal) < d){
        if(OldVal>NewVal){
          profile.addToProfile(NewVal + d, Math.abs(OldVal - NewVal), -a)
          profile.addToProfile(NewVal, Math.abs(OldVal - NewVal), a)

          updateVarViolation(NewVal+d,Math.abs(OldVal-NewVal))
          updateVarViolation(NewVal,Math.abs(OldVal-NewVal))
        }else{
          profile.addToProfile(OldVal + d, Math.abs(OldVal - NewVal), a)
          profile.addToProfile(OldVal, Math.abs(OldVal - NewVal), -a)
          updateVarViolation(OldVal+d,Math.abs(OldVal-NewVal))
          updateVarViolation(OldVal,Math.abs(OldVal-NewVal))
        }
      }else {
        profile.addToProfile(OldVal, d, -a)
        profile.addToProfile(NewVal, d, a)
        updateVarViolation(OldVal, d)
        updateVarViolation(NewVal, d)
      }
    } else if (duration(index) == v && amount(index).value > 0) {
      //duration
      val AdjustedOldVal = Math.max(0,OldVal)
      val AdjustedNewVal = Math.max(0,NewVal)
      if (AdjustedOldVal > AdjustedNewVal) {
        profile.addToProfile(AdjustedNewVal + start(index).value, AdjustedOldVal - AdjustedNewVal, -amount(index).value)
        updateVarViolation(AdjustedNewVal + start(index).value, AdjustedOldVal - AdjustedNewVal)
      } else {
        profile.addToProfile(AdjustedOldVal + start(index).value, AdjustedNewVal - AdjustedOldVal, amount(index).value)
        updateVarViolation(AdjustedOldVal + start(index).value, AdjustedNewVal - AdjustedOldVal)
      }
    } else if(amount(index) == v){
      profile.addToProfile(start(index).value, duration(index).value, NewVal - OldVal)
      updateVarViolation(start(index).value,duration(index).value)
    }
    //println(violation)
  }

  override def checkInternals(c: Checker) {c.check(verity = false, Some("TODO: Implement checkinternal for CumulativeSparse"))}
}

class CumulativeProfile(m:Store, val nTasks:Int, val horizon:(Int,Int), val maxHeight:Int, var limit:IntValue){
  val horizonStart = horizon._1
  val horizonEnd = horizon._2

  val blocks:Array[ProfileBlock] = Array.tabulate(2*nTasks+4)( i => new ProfileBlock(horizonStart,horizonStart,CBLSIntVar(m,0,0 to maxHeight),limit,horizon))

  val freeBlocks: ProfileBlock = blocks(0)
  for( i <- 1 until blocks.length)
    freeBlocks.insertBlock(blocks(i))

  val endBlock = new ProfileBlock(horizonStart-2,horizonStart-3,CBLSIntVar(m,-2,-3 to -2,"DummyEndBlock"),limit,horizon,true)
  val profile = new ProfileBlock(horizonStart-1,horizonStart-2,CBLSIntVar(m,-1,-2 to -1,"DummyBlock"),limit,horizon,true)
  profile.insertBlock(endBlock)
  val initialBlock: ProfileBlock = freeBlocks.popNext()
  initialBlock.setProfile(horizonStart,horizonEnd,0)
  profile.insertBlock(initialBlock)

  def printProfile(): Unit = {
    println("-------------")
    var idx = 0
    var current = profile
    while(current != endBlock){
//      for(i <- current.start to current.end) {
//        val p = Array.tabulate(current.height.newValue)(n => "#")
//        println(idx+"|"+i + " " +current.start+" to "+ current.end +" \t |" +/*p.mkString +*/ current.height.newValue)
//      }

      if(current.start > current.end){
        println(idx+"!"+current.start+" to "+ current.end + " \t !" +/*p.mkString +*/ current.height.newValue)
      }else{
        println(idx+"|" + " " +current.start+" to "+ current.end +" \t |" +/*p.mkString +*/ current.height.newValue)
      }
      current = current.next
      idx = idx+1
    }
  }

  def addToProfile(start:Int, duration:Int, height:Int): Unit = {
    if(duration > 0 && height != 0) {
      if (freeBlocks.next == null) {
        printProfile()
      }
//      println("@@@@@@@@" +  s"$start $duration $height" )
//      printProfile()

      var currentProfile = profile.next

      if(currentProfile.start > start){
        println("Something is wrong here")
      }

      while (currentProfile != endBlock && !currentProfile.contains(start)) currentProfile = currentProfile.next

      currentProfile.changeInterval(start, start + duration - 1, height, freeBlocks)
    }else if(duration < 0){
      System.err.println("% Warning: Duration of task less than 0 in cumulative")
    }

  }

  def getViolation(start:Int, duration:Int):Int = {
    if(duration < 1){
      return 0
    }
    val end = start+duration-1
    var currentProfile = profile.next
    while(currentProfile != endBlock && ! currentProfile.contains(start)) currentProfile = currentProfile.next

    var total = 0
    var currentStart = start
    while(currentProfile.contains(currentStart) && currentStart <= end){
      val tmpEnd = Math.min(end,currentProfile.end)
      total = total + (tmpEnd-currentStart+1)*currentProfile.getInternalOverLimit
      currentStart = tmpEnd + 1
      currentProfile = currentProfile.next
    }

    total
  }
}



//Note that a block where start = end -> width = 1
class ProfileBlock(private[this] var _start:Int, private[this] var _end:Int, var height:CBLSIntVar, val limit:IntValue, horizon:(Int,Int), val immutable:Boolean = false ){
  var prev:ProfileBlock = null
  var next:ProfileBlock = null

  val width = CBLSIntVar(height.model, horizon._1, horizon._1 to horizon._2 )
  val overLimit = MinusOffsetPos(height,limit,0)
  val blockViolation = Prod2(width,overLimit)

  def start: Int = _start
  def start_=(newStart:Int): Unit = {
    _start = newStart
    width := _end-_start+1
  }

  def end: Int = _end
  def end_=(newEnd:Int): Unit = {
    _end = newEnd
    width := _end-_start+1
  }

  def getInternalOverLimit:Int = {
    Math.max(0, height.newValue-limit.value)
  }
  def insertBlock(block:ProfileBlock): Unit = {
    block.next = next
    block.prev = this
    if(next != null)
      block.next.prev = block
    next = block
  }

  def removeBlock(): Unit = {
    if(immutable) {
      println("% Warning: Attempted to remove an immutable block.")
    }
    prev.next = next
    if(next != null)
      next.prev = prev
    reset()
  }
  def popNext():ProfileBlock = {
    val tmp = next
    next.removeBlock()
    tmp
  }

  def setProfile(newStart:Int, newEnd:Int, newHeight:Int): Unit = {
    _start = newStart
    _end = newEnd
    height := newHeight
    width := _end-_start+1
  }

  //Precondition: iStart >= start and iStart <= end
  def changeInterval(iStart:Int, iEnd:Int, iHeight:Int, freeBlocks:ProfileBlock):Unit = {
    assert(contains(iStart))

    if(iStart == start){
      if(iEnd == end){
        changeEntireBlock(iHeight,freeBlocks)
      }else if(iEnd < end){
        changeStartOfBlock(iEnd,iHeight,freeBlocks)
      }else{
        //Carry over into the next block
        val nextStart = end + 1
        val tmp = next
        changeEntireBlock(iHeight,freeBlocks)
        tmp.changeInterval(nextStart,iEnd,iHeight,freeBlocks)
      }
    }else{
      if(iEnd == end){
        changeEndOfBlock(iStart,iHeight,freeBlocks)
      }else if(iEnd < end){
        splitBlock(iStart,iEnd,iHeight,freeBlocks)
      }else{
        //Carry over into the next block
        val nextStart = end + 1
        val tmp = next
        changeEndOfBlock(iStart,iHeight,freeBlocks)
        //If we merged with the next block
        tmp.changeInterval(nextStart,iEnd,iHeight,freeBlocks)
      }
    }
  }

  def changeEntireBlock(iHeight:Int, freeBlocks:ProfileBlock): Unit = {
    if(prev.height.newValue == next.height.newValue && prev.height.newValue == height.newValue+iHeight){
      //Merge left and right block
      next.start = prev.start
      val tmp = prev
      tmp.removeBlock()
      freeBlocks.insertBlock(tmp)
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else if(prev.height.newValue == height.newValue+iHeight){
      //Merge with left block
      prev.end = end
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else if(next.height.newValue == height.newValue+iHeight){
      //Merge with right block
      next.start = start
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else{
      height :+= iHeight
    }
  }

  def changeStartOfBlock(iEnd:Int, iHeight:Int, freeBlocks:ProfileBlock): Unit = {
    if(prev.height.newValue == height.newValue+iHeight){
      prev.end = iEnd
      start = iEnd + 1
    }else{
      val newBlock = freeBlocks.popNext()
      newBlock.setProfile(start,iEnd,height.newValue+iHeight)
      start = iEnd + 1
      prev.insertBlock(newBlock)
    }
  }

  def changeEndOfBlock(iStart:Int, iHeight:Int, freeBlocks:ProfileBlock): Unit = {
    if(next.height.newValue == height.newValue+iHeight){
      next.start = iStart
      end = iStart - 1
    }else{
      val newBlock = freeBlocks.popNext()
      newBlock.setProfile(iStart,end,height.newValue+iHeight)
      end = iStart - 1
      insertBlock(newBlock)
    }
  }

  def splitBlock(iStart:Int, iEnd:Int, iHeight:Int, freeBlocks:ProfileBlock): Unit = {
    val middleBlock = freeBlocks.popNext()
    val endBlock = freeBlocks.popNext()
    endBlock.setProfile(iEnd+1,end,height.newValue)
    middleBlock.setProfile(iStart,iEnd,height.newValue+iHeight)
    this.end = iStart-1
    this.insertBlock(endBlock)
    this.insertBlock(middleBlock)
  }

  def contains(x:Int):Boolean = {start <= x && x <= end}

  def reset():Unit = {
    start = 0
    end = 0
    height := 0
  }

}

