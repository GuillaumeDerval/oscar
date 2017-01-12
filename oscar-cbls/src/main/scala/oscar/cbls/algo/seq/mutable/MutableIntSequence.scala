package oscar.cbls.algo.seq.mutable

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

class MutableIntSequence extends Iterable[SeqPosition]{
  var headPhantom = new SymSeqPosition(null,null,-1){
    override def toString : String = "headPhantom" + super.toString
  }
  var tailPhantom = new SymSeqPosition(null,null,-2){
    override def toString : String = "tailPhantom" + super.toString
  }

  headPhantom.a = tailPhantom
  tailPhantom.b = headPhantom

  def headPhantomPosition = new SeqPosition(headPhantom,false)
  def tailPhantomPosition = new SeqPosition(tailPhantom,false)

  protected[this] var mSize = 0
  override def size:Int = mSize

  protected def createNewSymPos(value:Int):SymSeqPosition = new SymSeqPosition(null,null,value)
  protected def freeSymPos(pos:SymSeqPosition){}

  override def head:SeqPosition = {
    if(isEmpty) throw new Error(".head on empty Int Sequence")
    val theNeighbor = headPhantom.a
    new SeqPosition(theNeighbor, (theNeighbor.a == headPhantom))
  }

  override def last:SeqPosition= {
    if(isEmpty) throw new Error(".last on empty Int Sequence")
    val theNeighbor = tailPhantom.b
    new SeqPosition(theNeighbor, (theNeighbor.b == tailPhantom))
  }

  override def isEmpty: Boolean = (mSize == 0)


  override def toString() : String = {
    this.getClass.getSimpleName + "(size:" + this.size + ")(" + this.toIntList + ")"
  }

  override def iterator: Iterator[SeqPosition] = new MutableIntSequenceIterator(headPhantomPosition)
  def toIntList:List[Int] = this.map(x => x.pos.value).toList //TODO: reduce overhead

  /**
   *
   * @param value
   * @param pos is pos is null, inserts at the first position in the seq (useful for inserting the first element...)
   * @return
   */
  def insertAfter(value:Int, pos:SeqPosition = null):SeqPosition = {
    if(pos == null){
      val oldNext = headPhantom.a
      val newPos = new SeqPosition(createNewSymPos(value))
      newPos.pos.a = oldNext
      newPos.pos.b = headPhantom
      headPhantom.replace(oldNext,newPos.pos)
      oldNext.replace(headPhantom,newPos.pos)
      mSize += 1
      newPos
    }else {
      val oldNext = pos.next
      val newPos = new SeqPosition(createNewSymPos(value))
      pos.setNextAndRelink(newPos)
      newPos.setNextAndRelink(oldNext)
      mSize += 1
      newPos
    }
  }

  /**
   *
   * @param value
   * @param pos is pos is null, inserts at the last position in the seq (useful for inserting the first element...)
   * @return
   */
  def insertBefore(value:Int, pos:SeqPosition = null):SeqPosition = {
    if (pos == null) {
      val oldPrev = tailPhantom.b
      val newPos = new SeqPosition(createNewSymPos(value))
      newPos.pos.a = tailPhantom
      newPos.pos.b = oldPrev
      tailPhantom.replace(oldPrev,newPos.pos)
      oldPrev.replace(tailPhantom,newPos.pos)
      mSize += 1
      newPos
    } else {
      val oldPrev = pos.prev
      val newPos = new SeqPosition(createNewSymPos(value))
      oldPrev.setNextAndRelink(newPos)
      newPos.setNextAndRelink(pos)
      mSize += 1
      newPos
    }
  }

  def insertBetween(value:Int, ba:SymSeqPosition,bb:SymSeqPosition):SymSeqPosition = {
    val newPos = createNewSymPos(value)
    ba.replace(bb,newPos)
    bb.replace(ba,newPos)
    newPos.a = ba
    newPos.b = bb
    mSize += 1
    newPos
  }


  def delete(pos:SeqPosition){delete(pos.pos)}
  def delete(pos:SymSeqPosition){
    pos.a.replace(pos,pos.b)
    pos.b.replace(pos,pos.a)
    mSize -=1
    freeSymPos(pos)
  }

  /** supposed to be in the same intSequence
    *
    * @param start
    * @param end
    * @param moveAfterTarget
    * @param flip
    */
  def moveAfter(start:SeqPosition, end:SeqPosition, moveAfterTarget:SeqPosition, flip:Boolean){
    //perform the cut
    val oldPrev = start.prev
    val oldNext = end.next
    oldPrev.setNextAndRelink(oldNext)
    //perform the flip, if needed
    val (newStart,newEnd) = if(flip){
      end.flip
      start.flip
      ((end,start))
    }else ((start,end))

    //perform the insert
    val newNext = moveAfterTarget.next
    moveAfterTarget.setNextAndRelink(newStart)
    newEnd.setNextAndRelink(newNext)
  }

  def moveAfter(moved:SymSeqPosition, moveAfterTarget:SeqPosition){
    val a = moved.a
    val b = moved.b
    a.replace(moved,b)
    b.replace(moved,a)
    val moveAfterTargetNextPos = moveAfterTarget.next.pos

    moveAfterTarget.pos.replace(moveAfterTargetNextPos,moved)
    moveAfterTargetNextPos.replace(moveAfterTarget.pos,moved)
    moved.a = moveAfterTarget.pos
    moved.b = moveAfterTargetNextPos
  }

  def moveBetween(moved:SymSeqPosition, ba:SymSeqPosition, bb:SymSeqPosition){
    val a = moved.a
    val b = moved.b
    a.replace(moved,b)
    b.replace(moved,a)

    ba.replace(bb,moved)
    bb.replace(ba,moved)
    moved.a = ba
    moved.b = bb
  }

  def checkInternals{
    var prevElement:SymSeqPosition = headPhantom
    var currentElement:SymSeqPosition = headPhantom.a
    while(currentElement!= null){
      require(prevElement == currentElement.a || prevElement == currentElement.b)
      require(prevElement.a == currentElement || prevElement.b == currentElement)
      val tmp1 = prevElement
      prevElement = currentElement
      currentElement = currentElement.otherAdjacent(tmp1)
    }
    require(prevElement == tailPhantom)
    require(headPhantom.b == null)
    require(tailPhantom.a == null)
    require(headPhantom.a != null)
    require(tailPhantom.b != null)
    require(this.toList.size == this.size)
  }
}

class MutableUniqueIntSequence(maxVal:Int) extends MutableIntSequence{

  private val valuesTopos:Array[SymSeqPosition] = Array.fill[SymSeqPosition](maxVal+1)(null)

  override protected def freeSymPos(pos:SymSeqPosition){
    assert(valuesTopos(pos.value) != null)
    valuesTopos(pos.value) = null
  }

  override protected def createNewSymPos(value:Int): SymSeqPosition ={
    require(valuesTopos(value) == null,"UniqueIntSequence requires that each value appears at most once, duplicate use of " + value)
    val newSymPos = new SymSeqPosition(null,null,value)
    valuesTopos(value) = newSymPos
    newSymPos
  }

  def symPosAt(value:Int):SymSeqPosition = valuesTopos(value)
}

class MutableIntSequenceIterator(var currentPosition:SeqPosition) extends Iterator[SeqPosition]{
  override def hasNext: Boolean = currentPosition.hasNext

  override def next(): SeqPosition = {
    currentPosition = currentPosition.next
    currentPosition
  }
}

class SymSeqPosition(var a:SymSeqPosition, var b:SymSeqPosition, val value:Int){
  def replace(oldPos:SymSeqPosition,newPos:SymSeqPosition){
    if(oldPos == a){
      a = newPos
    }else{
      b = newPos
    }
  }

  override def toString : String = "SymSeqPosition(a,b," + value + ")"

  def otherAdjacent(that:SymSeqPosition):SymSeqPosition = {
    if(a == that) b else a
  }
}

class SeqPosition(val pos:SymSeqPosition, var headIsOnA:Boolean = true){
  implicit def toPos:SymSeqPosition = pos

  def flip {headIsOnA = !headIsOnA}
  /**does not perform the cut*/
  def setNextAndRelink(newNext:SeqPosition){
    if(headIsOnA) pos.b = newNext.pos else pos.a = newNext.pos
    if(newNext.headIsOnA) newNext.pos.a = pos else newNext.pos.b = pos
  }

  /**does not perform the cut*/
  def setPrevAndRelink(newPrev:SeqPosition){
    if(headIsOnA) pos.a = newPrev.pos else pos.b = newPrev.pos
    if(newPrev.headIsOnA) newPrev.pos.b = pos else newPrev.pos.a = pos
  }

  //null if no such position exist
  def next:SeqPosition = {
    val theNeighbor = if (headIsOnA) pos.b else pos.a
    if(theNeighbor.b == null) return null
    new SeqPosition(theNeighbor, (theNeighbor.a == pos))
  }

  def hasNext:Boolean = {
    val theNeighbor = if (headIsOnA) pos.b else pos.a
    (theNeighbor.a != null)
  }

  //null if no such position exist
  def prev:SeqPosition = {
    val theNeighbor = if (headIsOnA) pos.a else pos.b
    if(theNeighbor.a == null) return null
    new SeqPosition(theNeighbor, (theNeighbor.b == pos))
  }

  def hasPrev:Boolean = {
    val theNeighbor = if (headIsOnA) pos.a else pos.b
    (theNeighbor.b != null)
  }

  override def toString : String = "SeqPosition(pos:" + pos + " headIsOnA:" + headIsOnA + ")"
}

object testSeq extends App{
  val a = new MutableIntSequence

  println(a)

  for(i <- 0 to 10){
    a.insertBefore(i)
  }

  println(a)

  a.checkInternals

  val pos3 = a.head.next.next.next
  println(pos3)

  val pos6 = pos3.next.next.next
  println(pos6)

  a.moveAfter(pos3,pos6,a.head,true)
  println(a)
  a.checkInternals

  val posa = a.head.next.next.next
  val posb = pos3.next.next.next
  a.moveAfter(posa,posb,a.head.next,true)
  a.checkInternals

  println(a)
}

