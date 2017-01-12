package oscar.cbls.algo.seq.functional

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

import oscar.cbls.algo.fun.{LinearTransform, PiecewiseLinearBijectionNaive, Pivot}
import oscar.cbls.algo.lazyIt.LazyFilter
import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}

import scala.language.implicitConversions

object UniqueIntSequence{
  def apply(values:Iterable[Int]):UniqueIntSequence = {
    var toReturn = empty()
    for (i <- values) {
      toReturn = toReturn.insertAtPosition(i, toReturn.size, false, false)
    }
    toReturn
  }

  def empty():UniqueIntSequence = new ConcreteUniqueIntSequence(
    RedBlackTreeMap.empty[Int],
    RedBlackTreeMap.empty[Int],
    PiecewiseLinearBijectionNaive.identity,
    0
  )


  implicit def toIterable(seq:UniqueIntSequence):IterableUniqueIntSequence = new IterableUniqueIntSequence(seq)

  private var nextUniqueID:Int = Int.MinValue
  def getNewUniqueID():Int = {
    nextUniqueID +=1
    nextUniqueID
  }
}

class IterableUniqueIntSequence(sequence:UniqueIntSequence) extends Iterable[Int]{
  override def iterator : Iterator[Int] = sequence.iterator

  override def head : Int = sequence.valueAtPosition(0).head

  override def headOption : Option[Int] = sequence.valueAtPosition(0)

  override def last : Int = sequence.valueAtPosition(sequence.size-1).head

  override def lastOption : Option[Int] = sequence.valueAtPosition(sequence.size-1)
}

abstract class UniqueIntSequence(protected[seq] val uniqueID:Int = UniqueIntSequence.getNewUniqueID()) {

  def size : Int
  def isEmpty:Boolean
  def iterator : Iterator[Int] = new UniqueIntSequenceIterator(this)
  def iterable : Iterable[Int] = new IterableUniqueIntSequence(this)
  def unorderedContent:Iterable[Int]
  //  def largestValue:Option[Int]
  //  def smallestValue:Option[Int]

  def valueAtPosition(position : Int) : Option[Int]
  def positionOfValue(value : Int) : Option[Int]
  def contains(value:Int):Boolean

  def explorerAtPosition(position : Int) : Option[UniqueIntSequenceExplorer]
  def explorerAtValue(value: Int) : Option[UniqueIntSequenceExplorer] =  {
    positionOfValue(value) match{
      case None => None
      case Some(pos) => explorerAtPosition(pos)
    }
  }

  def insertAtPosition(value:Int, pos:Int, fast:Boolean = false, autoRework:Boolean = true):UniqueIntSequence
  def delete(pos:Int, fast:Boolean=false,autoRework:Boolean = false):UniqueIntSequence
  def moveAfter(startPositionIncluded:Int, endPositionIncluded:Int, moveAfterPosition:Int, flip:Boolean, fast:Boolean = false, autoRework:Boolean = true):UniqueIntSequence

  def regularize(targetUniqueID:Int = this.uniqueID):ConcreteUniqueIntSequence
  def commitPendingMoves:UniqueIntSequence

  def check{}

  def quickEquals(that:UniqueIntSequence):Boolean = that != null && this.uniqueID == that.uniqueID
  def equals(that:UniqueIntSequence):Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString : String = {
    "UniqueIntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]_impl:" + descriptorString
  }

  def descriptorString : String

  def predecessorVal2Val(value:Int):Option[Int] = {
    explorerAtValue(value) match{
      case None => None
      case Some(x) => x.prev match{
        case Some(y) => Some(y.value)
        case None => None
      }
    }
  }

  def predecessorPos2Val(position:Int):Option[Int] = {
    valueAtPosition(position-1)
  }

  def successorVal2Val(value:Int):Option[Int] = {
    explorerAtValue(value) match{
      case None => None
      case Some(x) => x.next match{
        case Some(y) => Some(y.value)
        case None => None
      }
    }
  }

  def successorPos2Val(position:Int):Option[Int] = {
    valueAtPosition(position+1)
  }
}

class ConcreteUniqueIntSequence(private[seq] val internalPositionToValue:RedBlackTreeMap[Int],
                                private[seq] val valueToInternalPosition:RedBlackTreeMap[Int],
                                private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                                private[seq] val startFreeRangeForInternalPosition:Int,
                                uniqueID:Int = UniqueIntSequence.getNewUniqueID()) extends UniqueIntSequence(uniqueID){

  override def descriptorString : String = "[" + this.unorderedContent.mkString(",") + "]"

  override def toString : String = {
    "UniqueIntSequence(size:" + size + ")[" + this.iterator.toList.mkString(",") + "]_impl:concrete"
  }

  override def check {
    require(internalPositionToValue.content.sortBy(_._1) equals valueToInternalPosition.content.map({case (a,b) => (b,a)}).sortBy(_._1))
  }

  def size : Int = valueToInternalPosition.size
  def isEmpty:Boolean = internalPositionToValue.isEmpty

  def largestValue:Option[Int] = valueToInternalPosition.biggest match{case None => None case Some((k,_)) => Some(k)}
  def smallestValue:Option[Int] = valueToInternalPosition.smallest match{case None => None case Some((k,_)) => Some(k)}
  def contains(value:Int):Boolean = valueToInternalPosition.contains(value)

  def valueAtPosition(position : Int) : Option[Int] = {
    val internalPosition:Int = externalToInternalPosition.forward(position)
    internalPositionToValue.get(internalPosition)
  }

  def positionOfValue(value : Int) : Option[Int] = {
    valueToInternalPosition.get(value) match{
      case None => None
      case Some(internalPosition) => Some(externalToInternalPosition.backward(internalPosition))
    }
  }

  def explorerAtPosition(position : Int) : Option[UniqueIntSequenceExplorer] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.forward.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition:Option[RedBlackTreeMapExplorer[Pivot]],internalPosition) = currentPivotPosition match {
        case None => (externalToInternalPosition.forward.firstPivotAndPosition,position)
        case Some(p) => (p.next,p.value.f(position))
      }

      Some(new ConcreteUniqueIntSequenceExplorer(this,
        position,
        internalPositionToValue.positionOf(internalPosition).head,
        currentPivotPosition,
        pivotAbovePosition)()
      )
    }
  }

  def insertAtPosition(value:Int, pos:Int, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {

    //println(this + ".insertAtPosition(value:" + value + " pos:" + pos + ")")
    require(pos<=size,"inserting past the end of the sequence (size:" + size + " pos:" + pos + ")")

    if(fast) return new InsertedUniqueIntSequence(this,value,pos)

    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition,value)
    val newValueToInternalPosition = valueToInternalPosition.insert(value,startFreeRangeForInternalPosition)

    //move sequence after position, one upward
    //move inserted point at its position
    val oldExternalPosRelatedToFreeInternalPos = externalToInternalPosition.backward(startFreeRangeForInternalPosition)

    val newExternalToInternalPosition = if(pos == size) {
      //inserting at end of the sequence
      externalToInternalPosition.updateBefore(
        (size,size,LinearTransform(oldExternalPosRelatedToFreeInternalPos-pos,false)))
      //TODO: this migfht be always identity, actually, so useless!
    }else{
      //inserting somewhere within the sequence, need to shift upper part
      externalToInternalPosition.updateBefore(
        (pos+1,size,LinearTransform(-1,false)),
        (pos,pos,LinearTransform(oldExternalPosRelatedToFreeInternalPos-pos,false)))
    }

    new ConcreteUniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1)
  }

  def delete(pos:Int, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {
    //println(this + ".delete(pos:" + pos + ")")
    require(pos<size,"deleting past the end of the sequence (size:" + size + " pos:" + pos + ")")
    require(pos>=0,"deleting at negative pos:" + pos)

    if(fast) return new DeletedUniqueIntSequence(this,pos)

    val internalPosition = externalToInternalPosition(pos)
    val value = internalPositionToValue.get(internalPosition).head
    val largestInternalPosition = startFreeRangeForInternalPosition-1

    val valueAtLargestInternalPosition = internalPositionToValue.get(largestInternalPosition).head

    val newInternalPositionToValue = internalPositionToValue.
      insert(internalPosition,valueAtLargestInternalPosition).
      remove(largestInternalPosition)

    val newValueToInternalPosition = valueToInternalPosition.
      insert(valueAtLargestInternalPosition,internalPosition).
      remove(value)

    //now, update the fct knowing the move and remove
    val externalPositionAssociatedToLargestInternalPosition = externalToInternalPosition.backward(largestInternalPosition)

    //TODO: this is overly complex.
    val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
      (externalPositionAssociatedToLargestInternalPosition,
        externalPositionAssociatedToLargestInternalPosition,
        LinearTransform(pos-externalPositionAssociatedToLargestInternalPosition,false)),
      (pos,pos,LinearTransform(externalPositionAssociatedToLargestInternalPosition - pos,false))).updateBefore(
      (pos,size-2,LinearTransform(1,false)),(size-1,size-1,LinearTransform(pos - size + 1,false)))

    new ConcreteUniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition - 1)
  }

  def moveAfter(startPositionIncluded:Int, endPositionIncluded:Int, moveAfterPosition:Int, flip:Boolean, fast:Boolean,autoRework:Boolean):UniqueIntSequence = {
    //println(this + ".moveAfter(startPositionIncluded:" + startPositionIncluded + " endPositionIncluded:" + endPositionIncluded + " moveAfterPosition:" + moveAfterPosition + " flip:" + flip + ")")
    require(startPositionIncluded >= 0 && startPositionIncluded < size , "startPositionIncluded should be in [0,size[ in UniqueIntSequence.moveAfter")
    require(endPositionIncluded >= 0 && endPositionIncluded < size , "endPositionIncluded should be in [0,size[ in UniqueIntSequence.moveAfter")
    require(moveAfterPosition >= 0 && moveAfterPosition < size , "moveAfterPosition should be in [0,size[ in UniqueIntSequence.moveAfter")

    require(
      moveAfterPosition < startPositionIncluded || moveAfterPosition>endPositionIncluded,
      "moveAfterPosition=" +  moveAfterPosition + " cannot be between startPositionIncluded=" + startPositionIncluded + " and endPositionIncluded=" + endPositionIncluded)
    require(startPositionIncluded <= endPositionIncluded, "startPositionIncluded=" + startPositionIncluded + " should be <= endPositionIncluded=" + endPositionIncluded)

    if(fast) return new MovedUniqueIntSequence(this,startPositionIncluded,endPositionIncluded,moveAfterPosition,flip)

    if(moveAfterPosition + 1 == startPositionIncluded) {
      //not moving
      if(flip) { //just flipping
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded,endPositionIncluded,LinearTransform(endPositionIncluded + startPositionIncluded,true)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)

      }else{
        this //nop
      }
    }else{
      if(moveAfterPosition > startPositionIncluded){ //move upwards
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (startPositionIncluded,
            moveAfterPosition + startPositionIncluded - endPositionIncluded -1,
            LinearTransform(endPositionIncluded + 1 - startPositionIncluded,false)),
          (startPositionIncluded + moveAfterPosition - endPositionIncluded,
            moveAfterPosition,
            LinearTransform(if(flip) startPositionIncluded + moveAfterPosition
            else endPositionIncluded - moveAfterPosition,flip)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)

      }else{ //move downwards
      val newExternalToInternalPosition = externalToInternalPosition.updateBefore(
          (moveAfterPosition+1,
            moveAfterPosition + endPositionIncluded  - startPositionIncluded + 1,
            LinearTransform(if(flip) endPositionIncluded + moveAfterPosition + 1 else startPositionIncluded - moveAfterPosition - 1,flip)),
          (moveAfterPosition + endPositionIncluded  - startPositionIncluded + 2,
            endPositionIncluded,
            LinearTransform(startPositionIncluded - endPositionIncluded - 1,false)))

        new ConcreteUniqueIntSequence(
          internalPositionToValue,
          valueToInternalPosition,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition)
      }
    }
  }

  def regularize(targetUniqueID:Int = this.uniqueID):ConcreteUniqueIntSequence = {
    //println("regularize")
    var content = this.iterator
    var explorer = this.explorerAtPosition(0)
    var newInternalPositionToValues = RedBlackTreeMap.empty[Int]
    var newValueToInternalPosition = RedBlackTreeMap.empty[Int]
    while(explorer match{
      case None => false
      case Some(position) =>
        newInternalPositionToValues = newInternalPositionToValues.insert(position.position,position.value)
        newValueToInternalPosition = newValueToInternalPosition.insert(position.value,position.position)
        explorer = position.next
        true
    }){}

    new ConcreteUniqueIntSequence(newInternalPositionToValues,
      newValueToInternalPosition,
      PiecewiseLinearBijectionNaive.identity,
      newInternalPositionToValues.size,targetUniqueID)
  }

  override def commitPendingMoves : UniqueIntSequence = this

  override def unorderedContent : Iterable[Int] = internalPositionToValue.values
}

class UniqueIntSequenceIterator(s:UniqueIntSequence) extends Iterator[Int] {
  var crawler = s.explorerAtPosition(0)

  override def hasNext : Boolean =
    crawler match{
      case None => false
      case Some(_) => true}

  override def next() : Int = {
    val position = crawler.head
    crawler = position.next
    position.value
  }
}

abstract class UniqueIntSequenceExplorer{
  def value:Int
  def position:Int
  def next:Option[UniqueIntSequenceExplorer]
  def prev:Option[UniqueIntSequenceExplorer]
}


class ConcreteUniqueIntSequenceExplorer(sequence:ConcreteUniqueIntSequence,
                                  override val position:Int,
                                  positionInRB:RedBlackTreeMapExplorer[Int],
                                  currentPivotPosition:Option[RedBlackTreeMapExplorer[Pivot]],
                                  pivotAbovePosition:Option[RedBlackTreeMapExplorer[Pivot]])(
                                   limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                                     case None => Int.MaxValue
                                     case Some(p) => p.value.fromValue-1},
                                   limitBelowForCurrentPivot:Int = currentPivotPosition match{
                                     case None => Int.MinValue
                                     case Some(p) => p.value.fromValue},
                                   slopeIsPositive:Boolean = currentPivotPosition match{
                                     case None => true
                                     case Some(p) => !p.value.f.minus}
                                   ) extends UniqueIntSequenceExplorer{

  override def toString : String = "ConcreteIntSequenceExplorer(position:" + position + " value:" + value + " currentPivotPosition:" + currentPivotPosition + " pivotAbovePosition:" + pivotAbovePosition + " positionInRB:" + positionInRB + ")"

  override val value : Int = positionInRB.value

  override def next : Option[UniqueIntSequenceExplorer] = {
    if(position == sequence.size-1) return None
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head
      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInRBOpt = sequence.internalPositionToValue.positionOf(pivotAbovePosition.head.value.f(newPosition))
      newPositionInRBOpt match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new ConcreteUniqueIntSequenceExplorer(sequence,
            newPosition,
            newPositionInRB,
            pivotAbovePosition,
            newPivotAbovePosition)(limitBelowForCurrentPivot = newPosition))
      }
    }else{
      //do not change pivot

      (if(slopeIsPositive) positionInRB.next else positionInRB.prev) match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new ConcreteUniqueIntSequenceExplorer(sequence,
            position + 1,
            newPositionInRB,
            currentPivotPosition,
            pivotAbovePosition)(
            limitAboveForCurrentPivot,
            limitBelowForCurrentPivot,
            slopeIsPositive))
      }
    }
  }

  override def prev : Option[UniqueIntSequenceExplorer] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot

      val newPosition = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition match{case None => newPosition case Some(position2) => position2.value.f(newPosition)}
      val newCurrentPositionInRB = sequence.internalPositionToValue.positionOf(newInternalPosition).head
      //println("change pivot newPosition:" + newPosition + " newCurrentPivotPosition:" + newCurrentPivotPosition + " oldPosition:" + currentPivotPosition)
      Some(new ConcreteUniqueIntSequenceExplorer(sequence,
        newPosition,
        newCurrentPositionInRB,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      //println("not change pivot")
      Some(new ConcreteUniqueIntSequenceExplorer(sequence,
        position-1,
        if(slopeIsPositive) positionInRB.prev.head else positionInRB.next.head,
        currentPivotPosition,
        pivotAbovePosition)(
        limitAboveForCurrentPivot,
        limitBelowForCurrentPivot,
        slopeIsPositive))
    }
  }
}

abstract class StackedUpdateUniqueIntSequence extends UniqueIntSequence(){
  override def delete(pos : Int, fast:Boolean,autoRework:Boolean) : UniqueIntSequence = {
    require(pos >= 0, "pos=" + pos + " for delete on UniqueIntSequence should be >= 0")
    require(pos < size, "cannot delete past end of sequence in UniqueIntSequence")
    new DeletedUniqueIntSequence(this,pos)
  }

  override def moveAfter(startPositionIncluded : Int, endPositionIncluded : Int, moveAfterPosition : Int, flip : Boolean, fast:Boolean,autoRework:Boolean) : UniqueIntSequence = {
    require(startPositionIncluded >= 0 && startPositionIncluded < size , "startPositionIncluded=" + startPositionIncluded + " should be in [0,size" + size + "[ in UniqueIntSequence.moveAfter")
    require(endPositionIncluded >= 0 && endPositionIncluded < size , "endPositionIncluded=" + endPositionIncluded +" should be in [0,size"+size+"[ in UniqueIntSequence.moveAfter")
    require(moveAfterPosition >= 0 && moveAfterPosition < size , "moveAfterPosition=" + moveAfterPosition + " should be in [0,size="+size+"[ in UniqueIntSequence.moveAfter")

    new MovedUniqueIntSequence(this,startPositionIncluded,endPositionIncluded,moveAfterPosition,flip)
  }

  override def insertAtPosition(value : Int, pos : Int, fast:Boolean,autoRework:Boolean) : UniqueIntSequence = {
    require(pos >= 0 && pos <= size , "pos=" + pos + " should be in [0,size="+size+"] in UniqueIntSequence.insertAt")
    new InsertedUniqueIntSequence(this,value:Int,pos:Int)
  }

  override def regularize(targetUniqueID:Int = this.uniqueID) : ConcreteUniqueIntSequence = commitPendingMoves.regularize(targetUniqueID)
}

class MovedUniqueIntSequence(val seq:UniqueIntSequence,
                             startPositionIncluded:Int,
                             endPositionIncluded:Int,
                             moveAfterPosition:Int,
                             flip:Boolean)
  extends StackedUpdateUniqueIntSequence{

  override def unorderedContent : Iterable[Int] = seq.unorderedContent

  override def descriptorString : String = seq.descriptorString + ".moved(startPos:" + startPositionIncluded + " endPos:" + endPositionIncluded + " targetPos:" + moveAfterPosition + " flip:" + flip + ")"

  val localBijection =
    if(moveAfterPosition + 1 == startPositionIncluded) {
      //not moving
      if(flip) { //just flipping
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (startPositionIncluded,endPositionIncluded,LinearTransform(endPositionIncluded + startPositionIncluded,true)))
      }else{
        PiecewiseLinearBijectionNaive.identity
      }
    }else {
      if (moveAfterPosition > startPositionIncluded) {
        //move upwards
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (startPositionIncluded, moveAfterPosition + startPositionIncluded - endPositionIncluded - 1,
            LinearTransform(endPositionIncluded + 1 - startPositionIncluded, false)),
          (startPositionIncluded + moveAfterPosition - endPositionIncluded,moveAfterPosition,
            LinearTransform(if (flip) startPositionIncluded + moveAfterPosition
            else endPositionIncluded - moveAfterPosition, flip)))
      } else {
        //move downwards
        PiecewiseLinearBijectionNaive.identity.updateBefore(
          (moveAfterPosition + 1, moveAfterPosition + endPositionIncluded - startPositionIncluded + 1,
            LinearTransform(if (flip) endPositionIncluded + moveAfterPosition + 1 else startPositionIncluded - moveAfterPosition - 1, flip)),
          (moveAfterPosition + endPositionIncluded - startPositionIncluded + 2, endPositionIncluded,
            LinearTransform(startPositionIncluded - endPositionIncluded - 1, false)))
      }
    }

  override val size : Int = seq.size

  override def commitPendingMoves:UniqueIntSequence = seq.commitPendingMoves.moveAfter(startPositionIncluded,endPositionIncluded,moveAfterPosition,flip,fast=false,autoRework = false)

  override def explorerAtPosition(position : Int) : Option[UniqueIntSequenceExplorer] = {
    val positionOfCurrentPivot = localBijection.forward.pivotWithPositionApplyingTo(position)
    seq.explorerAtPosition(localBijection.forward(position)) match{
      case None => None
      case Some(explorerInBasicSequence) =>
        Some(new MovedUniqueIntSequenceExplorer(this,position,
          explorerInBasicSequence,
          positionOfCurrentPivot,
          positionOfCurrentPivot match{case None => localBijection.forward.firstPivotAndPosition case Some(x) => x.next})())
    }
  }

  override def positionOfValue(value : Int) : Option[Int] = {
    seq.positionOfValue(value) match{
      case None => None
      case Some(intermediary) => Some(localBijection.backward(intermediary))
    }
  }

  override def contains(value : Int) : Boolean = seq.contains(value)

  override def isEmpty : Boolean = seq.isEmpty

  override def valueAtPosition(position : Int) : Option[Int] = {
    seq.valueAtPosition(localBijection.forward(position))
  }
}

class MovedUniqueIntSequenceExplorer(sequence:MovedUniqueIntSequence,
                               override val position:Int,
                               positionInBasicSequence:UniqueIntSequenceExplorer,
                               currentPivotPosition:Option[RedBlackTreeMapExplorer[Pivot]],
                               pivotAbovePosition:Option[RedBlackTreeMapExplorer[Pivot]])(
                                limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                                  case None => Int.MaxValue
                                  case Some(p) => p.value.fromValue-1},
                                limitBelowForCurrentPivot:Int = currentPivotPosition match{
                                  case None => Int.MinValue
                                  case Some(p) => p.value.fromValue},
                                slopeIsPositive:Boolean = currentPivotPosition match{
                                  case None => true
                                  case Some(p) => !p.value.f.minus}
                                ) extends UniqueIntSequenceExplorer{

  //  override def toString : String = "MovedIntSequenceExplorer(position:" + position + " value:" + value + " currentPivotPosition:" + currentPivotPosition + " pivotAbovePosition:" +
  //    pivotAbovePosition + " basicPositionn:" + positionInBasicSequence + ")"

  override val value : Int = positionInBasicSequence.value

  override def next : Option[UniqueIntSequenceExplorer] = {
    if(position == sequence.size-1) return None
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head
      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInBasicSequence = sequence.seq.explorerAtPosition(pivotAbovePosition.head.value.f(newPosition))
      newPositionInBasicSequence match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new MovedUniqueIntSequenceExplorer(sequence,
            newPosition,
            newPositionInRB,
            pivotAbovePosition,
            newPivotAbovePosition)(limitBelowForCurrentPivot = newPosition))
      }
    }else{
      //do not change pivot

      (if(slopeIsPositive) positionInBasicSequence.next else positionInBasicSequence.prev) match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new MovedUniqueIntSequenceExplorer(sequence,
            position + 1,
            newPositionInRB,
            currentPivotPosition,
            pivotAbovePosition)(
            limitAboveForCurrentPivot,
            limitBelowForCurrentPivot,
            slopeIsPositive))
      }
    }
  }

  override def prev : Option[UniqueIntSequenceExplorer] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot

      val newPosition = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition match{case None => newPosition case Some(position2) => position2.value.f(newPosition)}

      //println("change pivot newPosition:" + newPosition + " newCurrentPivotPosition:" + newCurrentPivotPosition + " oldPosition:" + currentPivotPosition)
      Some(new MovedUniqueIntSequenceExplorer(sequence,
        newPosition,
        sequence.seq.explorerAtPosition(newInternalPosition).head,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      //println("not change pivot")
      Some(new MovedUniqueIntSequenceExplorer(sequence,
        position-1,
        if(slopeIsPositive) positionInBasicSequence.prev.head else positionInBasicSequence.next.head,
        currentPivotPosition,
        pivotAbovePosition)(
        limitAboveForCurrentPivot,
        limitBelowForCurrentPivot,
        slopeIsPositive))
    }
  }
}

class InsertedUniqueIntSequence(seq:UniqueIntSequence,
                                val value:Int,
                                val pos:Int)
  extends StackedUpdateUniqueIntSequence {
  override val size : Int = seq.size + 1

  override def descriptorString : String = seq.descriptorString + ".inserted(val:" + value + " pos:" + pos + ")"

  override def unorderedContent : Iterable[Int] = seq.unorderedContent ++ List(value)

  override def positionOfValue(value : Int) : Option[Int] = {
    seq.positionOfValue(value) match {
      case None => if (value == this.value) Some(pos) else None
      case Some(p) => if (p < pos) Some(p) else Some(p + 1)
    }
  }

  override def explorerAtPosition(position : Int) : Option[UniqueIntSequenceExplorer] = {
    if (position == this.pos) {
      if (position == 0) {
        Some(new InsertedUniqueIntSequenceExplorer(this, position, seq.explorerAtPosition(0), true, true))
      } else {
        Some(new InsertedUniqueIntSequenceExplorer(this, position, seq.explorerAtPosition(position - 1), true, false))
      }
    } else if (position < this.pos) {
      seq.explorerAtPosition(position) match{
        case None => None
        case Some(p) => Some(new InsertedUniqueIntSequenceExplorer(this, position, Some(p), false, false))
      }
    } else {
      seq.explorerAtPosition(position-1) match{
        case None => None
        case Some(p) => Some(new InsertedUniqueIntSequenceExplorer(this, position, Some(p), false, false))
      }
    }
  }

  override def contains(value : Int) : Boolean = value == this.value || seq.contains(value)

  override def commitPendingMoves : UniqueIntSequence = seq.commitPendingMoves.insertAtPosition(value, pos, fast = false, autoRework = false)

  override def isEmpty : Boolean = false

  override def valueAtPosition(position : Int) : Option[Int] = {
    if (position == pos) Some(value)
    else if (position < pos) seq.valueAtPosition(position)
    else seq.valueAtPosition(position - 1)
  }
}


class InsertedUniqueIntSequenceExplorer(seq:InsertedUniqueIntSequence,
                                  val position:Int,
                                  explorerInOriginalSeq:Option[UniqueIntSequenceExplorer],
                                  atInsertedValue:Boolean,
                                  originalExplorerIsAbove:Boolean)
  extends UniqueIntSequenceExplorer {
  override def value : Int = if(atInsertedValue) seq.value else explorerInOriginalSeq.head.value

  override def next : Option[UniqueIntSequenceExplorer] = {
    if (atInsertedValue) {
      //we are leaving the inserted position
      explorerInOriginalSeq match{
        case None => None
        case Some(p) =>
          if (originalExplorerIsAbove) Some(new InsertedUniqueIntSequenceExplorer(seq, position + 1, explorerInOriginalSeq, atInsertedValue = false, originalExplorerIsAbove = false))
          else {
            p.next match {
              case None => None
              case Some(next1) =>
                Some(new InsertedUniqueIntSequenceExplorer(seq, position + 1, Some(next1), atInsertedValue = false, originalExplorerIsAbove = false))
            }
          }
      }
    }else {
      val nextPosition = position + 1
      if (nextPosition == seq.pos) {
        //we are getting into the inserted position
        Some(new InsertedUniqueIntSequenceExplorer(seq, position + 1, explorerInOriginalSeq, atInsertedValue = true, originalExplorerIsAbove = false))
      } else {
        //nothing special
        explorerInOriginalSeq.head.next match {
          case None => None
          case Some(next) => Some(new InsertedUniqueIntSequenceExplorer(seq, position + 1, Some(next), atInsertedValue = false, originalExplorerIsAbove = false))
        }
      }
    }
  }

  override def prev : Option[UniqueIntSequenceExplorer] = {
    if (atInsertedValue) {
      explorerInOriginalSeq match {
        case None => None
        case Some(p) =>
          //we are leaving the inserted position
          if (!originalExplorerIsAbove) Some(new InsertedUniqueIntSequenceExplorer(seq, position - 1, explorerInOriginalSeq, atInsertedValue = false, originalExplorerIsAbove = false))
          else {
            p.prev match {
              case None => None
              case Some(prev1) => Some(new InsertedUniqueIntSequenceExplorer(seq, position - 1, Some(prev1), atInsertedValue = false, originalExplorerIsAbove = false))
            }
          }
      }
    } else {
      val prevPosition = position - 1
      if (prevPosition == seq.pos) {
        //we are getting into the inserted position
        Some(new InsertedUniqueIntSequenceExplorer(seq, position - 1, explorerInOriginalSeq, atInsertedValue = true, originalExplorerIsAbove = true))
      } else {
        //nothing special
        explorerInOriginalSeq.head.prev match {
          case None => None
          case Some(prev) => Some(new InsertedUniqueIntSequenceExplorer(seq, position - 1, Some(prev), atInsertedValue = false, originalExplorerIsAbove = false))
        }
      }
    }
  }
}

class DeletedUniqueIntSequence(seq:UniqueIntSequence,
                               val position:Int)
  extends StackedUpdateUniqueIntSequence{

  val value = seq.valueAtPosition(position).head

  override def descriptorString : String = seq.descriptorString + ".deleted(pos:" + position + " val:" + value + ")"

  override def unorderedContent : Iterable[Int] = new LazyFilter(seq.unorderedContent,_ != value)

  override val size : Int = seq.size - 1

  override def explorerAtPosition(position : Int) : Option[UniqueIntSequenceExplorer] = {
    seq.explorerAtPosition(if (position < this.position) position else position + 1) match {
      case None => None
      case Some(e) => Some(new DeletedUniqueIntSequenceExplorer(this, position, e))
    }
  }

  override def positionOfValue(value : Int) : Option[Int] = {
    if(value == this.value) None
    else seq.positionOfValue(value) match{
      case None => None
      case Some(p) => if(p < this.position) Some(p) else Some(p-1)
    }
  }

  override def contains(value : Int) : Boolean = value != this.value && seq.contains(value)

  override def commitPendingMoves : UniqueIntSequence = seq.commitPendingMoves.delete(this.position,fast=false,autoRework=false)

  override def isEmpty : Boolean = false

  override def valueAtPosition(position : Int) : Option[Int] = {
    if(position >= this.position) seq.valueAtPosition(position+1)
    else seq.valueAtPosition(position)
  }
}

class DeletedUniqueIntSequenceExplorer(seq:DeletedUniqueIntSequence,
                                 val position:Int,
                                 explorerInOriginalSeq:UniqueIntSequenceExplorer)
  extends UniqueIntSequenceExplorer{
  override def value : Int = explorerInOriginalSeq.value

  override def prev : Option[UniqueIntSequenceExplorer] = {
    explorerInOriginalSeq.prev match {
      case None => None
      case Some(tentativePos) =>
        if(tentativePos.position == seq.position)
          tentativePos.prev match {
            case None => None
            case Some(secondTentativePos) => Some(new DeletedUniqueIntSequenceExplorer(seq, position - 1, secondTentativePos))
          }
        else Some(new DeletedUniqueIntSequenceExplorer(seq, position - 1, tentativePos))
    }
  }

  override def next : Option[UniqueIntSequenceExplorer] = {
    explorerInOriginalSeq.next match {
      case None => None
      case Some(tentativePos) =>
        if(tentativePos.position == seq.position)
          tentativePos.next match {
            case None => None
            case Some(secondTentativePos) => Some(new DeletedUniqueIntSequenceExplorer(seq, position + 1, secondTentativePos))
          }
        else Some(new DeletedUniqueIntSequenceExplorer(seq, position + 1, tentativePos))
    }
  }
}

