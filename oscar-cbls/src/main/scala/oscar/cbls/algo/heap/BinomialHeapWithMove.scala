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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.algo.heap

import oscar.cbls.core.propagation.Checker

import scala.collection.Iterator
import scala.collection.immutable.SortedMap

/**
 * This is a binary heap that is efficient; all operations are in O(log(n))
 * smallest first
 * @param initialGetKey a function that returns an integer for each element inserted in the heap this value is used to sort the heap content
 * @param maxsize the maximum number of elements that can be inserted in this heap
 * @param X the manifest of T, to create arrays of T's
 * @tparam T the type of elements included in the heap
 * @author renaud.delandtsheer@cetic.be
 */
class BinomialHeap[@specialized T](initialGetKey:T => Long,val maxsize:Long)(implicit val X:Manifest[T]) extends AbstractHeap[T] {
  var HeapArray:Array[T] = new Array[T](maxsize)
  private var msize:Long=0L

  var GetKey:T => Long = initialGetKey

  /**changes the key getter according to which the heap is sorted.
   * Can be costly if the heap is not empty.
   * @param KeyGetter the new key getter
   */
  def keyGetter_=(KeyGetter:T => Long){
    if(msize>0L){
      val content:List[T] = this.toList
      dropAll()
      GetKey = KeyGetter
      content foreach insert
    }else{
      GetKey = KeyGetter
    }
  }

  def keyGetter:(T => Long) = GetKey

  override def size = msize
  override def isEmpty:Boolean = msize == 0L

  override def toString():String = {
    HeapArray.toList.toString()
  }

  /**makes the datastruct empty, but does not frees the space*/
  override def dropAll(){
    msize = 0L
  }

  /**log(n)*/
  override def insert(elem:T){
    //insert en derniere position, puis bubble up
    HeapArray(msize)=elem
    msize +=1L
    pushUp(msize-1L)
  }

  /**O(1L) operation*/
  private def swapPositions(position1:Long,position2:Long){
    val tmp:T = HeapArray(position1)
    HeapArray(position1)=HeapArray(position2)
    HeapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Long):Long = {
    var position = startposition
    while(true)
      if(leftChild(position) < msize && GetKey(HeapArray(position)) > GetKey(HeapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < msize && GetKey(HeapArray(rightChild(position))) < GetKey(HeapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < msize && GetKey(HeapArray(position)) > GetKey(HeapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Long):Long = {
    var position = startposition
    while(true){
      val fatherposition:Long = father(position)
      if (fatherposition >= 0L && GetKey(HeapArray(position)) < GetKey(HeapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Long):Long = (position+1L)*2L-1L
  private def rightChild(position:Long):Long = (position+1L)*2L
  private def father(position:Long):Long = (position-1L)/2L

  /**O(firsts)*/
  override def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Long,acc:List[T]):List[T] = {
      if(startposition < msize && GetKey(HeapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),HeapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(msize == 0L)List.empty
    else ExploreFirsts(GetKey(HeapArray(0L)),0L,List.empty)
  }
  /**O(1L)*/
  override def getFirst:T=HeapArray(0L)

  /**O(log(n))*/
  override def popFirst():T={
    val toreturn:T = HeapArray(0L)
    if(msize == 1L){
      HeapArray(0L)=null.asInstanceOf[T]
      msize = 0L
    }else{
      swapPositions(0L,msize-1L)
      msize -=1L
      HeapArray(msize)=null.asInstanceOf[T]
      pushDown(0L)
    }
    toreturn
  }

  /**you cannot modify the hep when iterating through this iterator*/
  override def iterator: Iterator[T] = new BinomialHeapIterator(HeapArray,msize)

  override def popFirsts: List[T] = {
    if (isEmpty) return List.empty
    var acc = List(popFirst())
    val key = GetKey(acc.head)
    while(!isEmpty && GetKey(getFirst) == key){
      acc = popFirst() :: acc
    }
    acc
  }
}

class BinomialHeapIterator[T](HeapArray:Array[T],size:Long) extends Iterator[T]{
  var current:Long = size

  def hasNext: Boolean = current > 0L

  def next(): T = {
    current = current-1L
    HeapArray(current)
  }
}

/**
 * This is a binary heap that is less efficient than the [[oscar.cbls.algo.heap.BinomialHeap]].
 * It offers more operations, such as delete and update value.
 * smallest first
 * @param getKey a function that returns an integer for each element inserted i nthe heap this value is used to sort the heap content
 * @param maxsize the maximum number of elements that can be inserted in this heap
 * @param X the manifest of T, to create arrays of T's
 * @tparam T the type of elements included in the heap
 * @author renaud.delandtsheer@cetic.be
 * */
class BinomialHeapWithMove[T](getKey:T => Long,val maxsize:Long)(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  private[this] val heapArray:Array[T] = new Array[T](maxsize)
  var size:Long=0L
  var position:SortedMap[T,Long]=SortedMap.empty

  def isEmpty:Boolean = size == 0L

  def contains(value:T):Boolean = position.contains(value)

  def checkInternals(c:Checker){
    for(i <- heapArray.indices if i < size-1L){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }

    for(t <- position.keys){
      assert(heapArray(position(t)) == t)
    }
  }

  override def toString:String = {
    heapArray.toList.toString()
  }

  def insert(elem:T){
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position +=((elem,size))
    size +=1L
    pushUp(size-1L)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  private def swapPositions(position1:Long,position2:Long){
    position+=((heapArray(position1),position2))
    position+=((heapArray(position2),position1))

    val tmp:T = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Long):Long = {
    var position = startposition
    while(true)
      if(leftChild(position) < size && getKey(heapArray(position)) > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && getKey(heapArray(position)) > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Long):Long = {
    var position = startposition
    while(true){
      val fatherposition:Long = father(position)
      if (fatherposition >= 0L && getKey(heapArray(position)) < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Long):Long = (position+1L)*2L-1L
  private def rightChild(position:Long):Long =(position+1L)*2L
  private def father(position:Long):Long =  (position-1L)/2L

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Long,acc:List[T]):List[T] = {
      if(startposition < size && getKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0L)List.empty
    else ExploreFirsts(getKey(heapArray(0L)),0L,List.empty)
  }

  def getFirst:T=heapArray(0L)

  def removeFirst():T={
    val toreturn:T = heapArray(0L)
    swapPositions(0L,size-1L)
    size -=1L
    position -= toreturn
    heapArray(size)=null.asInstanceOf[T]
    pushDown(0L)
    toreturn
  }

  def notifyChange(elem:T){
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T){
    val startposition:Long = position(elem)
    if (startposition == size-1L){
      size -=1L
      position -= elem
      heapArray(size)=null.asInstanceOf[T]
    }else{
      swapPositions(startposition,size-1L)
      size -=1L
      position -= elem
      heapArray(size)=null.asInstanceOf[T]
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:T):Boolean = {
    position.get(elem) match{
      case None => false
      case Some(startposition) =>
        if (startposition == size-1L){
          size -=1L
          position -= elem
          heapArray(size)=null.asInstanceOf[T]
        }else{
          swapPositions(startposition,size-1L)
          size -=1L
          position -= elem
          heapArray(size)=null.asInstanceOf[T]
          pushDown(pushUp(startposition))
        }
        true
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param maxId
 */
class ArrayMap(maxId:Long) extends scala.collection.mutable.Map[Long, Long]{
  
  val array:Array[Long] = Array.fill[Long](maxId)(-1L)
  def get(key: Long): Option[Long] =  {
    val v = array(key)
    if(v == -1L) None
    else Some(array(key))
  }

  override def contains(key : Long) : Boolean = array(key) != -1L

  def iterator: Iterator[(Long, Long)] = {throw new Exception("enumeration not supported"); null}

  def +=(kv: (Long, Long)): this.type = {
    array(kv._1) = kv._2
    this
  }

  def -=(key: Long): this.type = {
    array(key) = -1L
    this
  }
}

/**
 * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
 * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
 * @author renaud.delandtsheer@cetic.be
 * @param GetKey
 * @param maxsize
 * @param position
 * @param A
 * @param X
 * @tparam T
 */
class BinomialHeapWithMoveExtMem[T](GetKey:T => Long,val maxsize:Long, position:scala.collection.mutable.Map[T,Long])(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  private[this] val HeapArray:Array[T] = new Array[T](maxsize)
  var size:Long=0L

  def checkInternals(c:Checker){
    for(i <- HeapArray.indices if i < size-1L){
      if (leftChild(i) < size){
        require(GetKey(HeapArray(i)) <= GetKey(HeapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(GetKey(HeapArray(i)) <= GetKey(HeapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = size == 0L

  override def toString:String = {
    HeapArray.toList.toString()
  }

  def insert(elem:T){
    //insert en derniere position, puis bubble up
    HeapArray(size)=elem
    position +=((elem,size))
    size +=1L
    pushUp(size-1L)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  def contains(value:T):Boolean = position.contains(value)

  private def swapPositions(position1:Long,position2:Long){
    position+=((HeapArray(position1),position2))
    position+=((HeapArray(position2),position1))

    val tmp:T = HeapArray(position1)
    HeapArray(position1)=HeapArray(position2)
    HeapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Long):Long = {
    var position = startposition
    val positionKey = GetKey(HeapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > GetKey(HeapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && GetKey(HeapArray(rightChild(position))) < GetKey(HeapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > GetKey(HeapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Long):Long = {
    var position = startposition
    val positionKey = GetKey(HeapArray(position))
    while(true){
      val fatherposition:Long = father(position)
      if (fatherposition >= 0L && positionKey < GetKey(HeapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Long):Long = (position+1L)*2L-1L
  @inline
  private def rightChild(position:Long):Long =(position+1L)*2L
  @inline
  private def father(position:Long):Long =  (position-1L)/2L

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Long,acc:List[T]):List[T] = {
      if(startposition < size && GetKey(HeapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),HeapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0L)List.empty
    else ExploreFirsts(GetKey(HeapArray(0L)),0L,List.empty)
  }

  def getFirst:T=HeapArray(0L)

  def removeFirst():T={
    val toreturn:T = HeapArray(0L)
    swapPositions(0L,size-1L)
    size -=1L
    position -= toreturn
    pushDown(0L)
    toreturn
  }

  def notifyChange(elem:T){
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T){
    val startposition:Long = position(elem)
    if (startposition == size-1L){
      size -=1L
      position -= elem
    }else{
      swapPositions(startposition,size-1L)
      size -=1L
      position -= elem
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:T):Boolean = {
    if(contains(elem)){
      delete(elem)
      true
    }else false
  }
}



/**
 * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
 * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
 * @author renaud.delandtsheer@cetic.be
 * @param getKey
 * @param maxsize
 * @param maxKey
 */
class BinomialHeapWithMoveInt(getKey:Long => Long,val maxsize:Long, val maxKey:Long){
  private[this] val heapArray:Array[Long] = new Array[Long](maxsize)

  private[this] val position:Array[Long] = Array.fill[Long](maxKey+1L)(-1L)

  var size:Long=0L

  def checkInternals(c:Checker){
    for(i <- heapArray.indices if i < size-1L){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = size == 0L

  override def toString:String = {
    heapArray.toList.toString()
  }

  def insert(elem:Long){
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position(elem) = size
    size +=1L
    pushUp(size-1L)
  }

  def contains(value:Long):Boolean = position(value) != -1L

  private def swapPositions(position1:Long,position2:Long){
    position(heapArray(position1)) = position2
    position(heapArray(position2)) = position1

    val tmp:Long = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Long):Long = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    require(false) //jamais execute
    position
  }

  private def pushUp(startposition:Long):Long = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true){
      val fatherposition:Long = father(position)
      if (fatherposition >= 0L && positionKey < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Long):Long = (position+1L)*2L-1L
  @inline
  private def rightChild(position:Long):Long =(position+1L)*2L
  @inline
  private def father(position:Long):Long =  (position-1L)/2L

  def getFirsts:List[Long] = {
    def ExploreFirsts(value:Long,startposition:Long,acc:List[Long]):List[Long] = {
      if(startposition < size && getKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0L)List.empty
    else ExploreFirsts(getKey(heapArray(0L)),0L,List.empty)
  }

  def getFirst:Long=heapArray(0L)

  def removeFirst():Long={
    val toreturn:Long = heapArray(0L)
    swapPositions(0L,size-1L)
    size -=1L
    position(toreturn) = -1L
    pushDown(0L)
    toreturn
  }

  def notifyChange(elem:Long){
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:Long){
    val startposition:Long = position(elem)
    if (startposition == size-1L){
      size -=1L
      position(elem) = -1L
    }else{
      swapPositions(startposition,size-1L)
      size -=1L
      position(elem) = -1L
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:Long):Boolean = {
    if(contains(elem)){
      delete(elem)
      true
    }else false
  }
}


