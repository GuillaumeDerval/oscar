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

package oscar.cbls.invariants.core.algo.heap

import oscar.cbls.invariants.core.propagation.Checker

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
class BinomialHeap[@specialized T](initialGetKey:T => Int,val maxsize:Int)(implicit val X:Manifest[T]) extends AbstractHeap[T] {
  var HeapArray:Array[T] = new Array[T](maxsize)
  private var msize:Int=0

  var GetKey:T => Int = initialGetKey

  /**changes the key getter according to which the heap is sorted.
   * Can be costly if the heap is not empty.
   * @param KeyGetter the new key getter
   */
  def keyGetter_=(KeyGetter:T => Int){
    if(msize>0){
      val content:List[T] = this.toList
      dropAll()
      GetKey = KeyGetter
      content map insert
    }else{
      GetKey = KeyGetter
    }
  }

  def keyGetter:(T => Int) = GetKey

  override def size = msize
  override def isEmpty:Boolean = (msize == 0)

  override def toString():String = {
    HeapArray.toList.toString()
  }

  /**makes the datastruct empty, but does not frees the space*/
  override def dropAll(){
    msize = 0
  }

  /**log(n)*/
  override def insert(elem:T){
    //insert en derniere position, puis bubble up
    HeapArray(msize)=elem
    msize +=1
    pushUp(msize-1)
  }

  /**O(1) operation*/
  private def swapPositions(position1:Int,position2:Int){
    val tmp:T = HeapArray(position1)
    HeapArray(position1)=HeapArray(position2)
    HeapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
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

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && GetKey(HeapArray(position)) < GetKey(HeapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Int):Int = (position+1)*2-1
  private def rightChild(position:Int):Int =(position+1)*2
  private def father(position:Int):Int = scala.math.floor((position+1)/2).toInt-1

  /**O(firsts)*/
  override def getFirsts:List[T] = {
    def ExploreFirsts(value:Int,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < msize && GetKey(HeapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),HeapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(msize == 0)List.empty
    else ExploreFirsts(GetKey(HeapArray(0)),0,List.empty)
  }
  /**O(1)*/
  override def getFirst:T=HeapArray(0)

  /**O(log(n))*/
  override def popFirst():T={
    val toreturn:T = HeapArray(0)
    if(msize == 1){
      HeapArray(0)=null.asInstanceOf[T]
      msize = 0
    }else{
      swapPositions(0,msize-1)
      msize -=1
      HeapArray(msize)=null.asInstanceOf[T]
      pushDown(0)
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

class BinomialHeapIterator[T](HeapArray:Array[T],size:Int) extends Iterator[T]{
  var current:Int = size

  def hasNext: Boolean = current > 0

  def next(): T = {
    current = current-1
    HeapArray(current)
  }
}

/**
 * This is a binary heap that is less efficient than the [[oscar.cbls.invariants.core.algo.heap.BinomialHeap]].
 * It offers more operations, such as delete and update value.
 * smallest first
 * @param getKey a function that returns an integer for each element inserted i nthe heap this value is used to sort the heap content
 * @param maxsize the maximum number of elements that can be inserted in this heap
 * @param X the manifest of T, to create arrays of T's
 * @tparam T the type of elements included in the heap
 * @author renaud.delandtsheer@cetic.be
 * */
class BinomialHeapWithMove[T](getKey:T => Int,val maxsize:Int)(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  private[this] val HeapArray:Array[T] = new Array[T](maxsize)
  var size:Int=0
  var position:SortedMap[T,Int]=SortedMap.empty

  def isEmpty = (size==0)

  def checkInternals(c:Checker){
    for(i <- HeapArray.indices if i < size-1){
      if (leftChild(i) < size){
        assert(getKey(HeapArray(i)) <= getKey(HeapArray(leftChild(i))),"heap error " + this + i)
        assert(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        assert(getKey(HeapArray(i)) <= getKey(HeapArray(rightChild(i))),"heap error " + this)
        assert(father(rightChild(i)) == i,"heap error " + this)
      }
    }

    for(t <- position.keys){
      assert(HeapArray(position(t)) == t)
    }
  }

  override def toString:String = {
    HeapArray.toList.toString()
  }

  def insert(elem:T){
    //insert en derniere position, puis bubble up
    HeapArray(size)=elem
    position +=((elem,size))
    size +=1
    pushUp(size-1)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  private def swapPositions(position1:Int,position2:Int){
    position+=((HeapArray(position1),position2))
    position+=((HeapArray(position2),position1))

    val tmp:T = HeapArray(position1)
    HeapArray(position1)=HeapArray(position2)
    HeapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    while(true)
      if(leftChild(position) < size && getKey(HeapArray(position)) > getKey(HeapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(HeapArray(rightChild(position))) < getKey(HeapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && getKey(HeapArray(position)) > getKey(HeapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && getKey(HeapArray(position)) < getKey(HeapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Int):Int = (position+1)*2-1
  private def rightChild(position:Int):Int =(position+1)*2
  private def father(position:Int):Int = scala.math.floor((position+1)/2).toInt-1

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Int,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < size && getKey(HeapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),HeapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(HeapArray(0)),0,List.empty)
  }

  def getFirst:T=HeapArray(0)

  def removeFirst():T={
    val toreturn:T = HeapArray(0)
    swapPositions(0,size-1)
    size -=1
    position -= toreturn
    HeapArray(size)=null.asInstanceOf[T]
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:T){
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T){
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position -= elem
      HeapArray(size)=null.asInstanceOf[T]
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position -= elem
      HeapArray(size)=null.asInstanceOf[T]
      pushDown(pushUp(startposition))
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param maxId
 */
class ArrayMap(maxId:Int) extends scala.collection.mutable.Map[Int, Int]{
  
  val array:Array[Int] = new Array[Int](maxId)
  def get(key: Int): Option[Int] =  Some(array(key))

  def iterator: Iterator[(Int, Int)] = {throw new Exception("enumeration not supported"); null}

  def +=(kv: (Int, Int)): this.type = {
    array(kv._1) = kv._2
    this
  }

  def -=(key: Int): this.type = {
    array(key) = null.asInstanceOf[Int]
    this
  }
}

/**
 * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
 * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
 * @author renaud.delandtsheer@cetic.be
 * @param getKey
 * @param maxsize
 * @param position
 * @param A
 * @param X
 * @tparam T
 */
class BinomialHeapWithMoveExtMem[T](getKey:T => Int,val maxsize:Int, position:scala.collection.mutable.Map[T,Int])(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  private[this] val HeapArray:Array[T] = new Array[T](maxsize)
  var size:Int=0

  def checkInternals(c:Checker){
    for(i <- HeapArray.indices if i < size-1){
      if (leftChild(i) < size){
        assert(getKey(HeapArray(i)) <= getKey(HeapArray(leftChild(i))),"heap error " + this + i)
        assert(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        assert(getKey(HeapArray(i)) <= getKey(HeapArray(rightChild(i))),"heap error " + this)
        assert(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = (size == 0)

  override def toString:String = {
    HeapArray.toList.toString()
  }

  def insert(elem:T){
    //insert en derniere position, puis bubble up
    HeapArray(size)=elem
    position +=((elem,size))
    size +=1
    pushUp(size-1)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  private def swapPositions(position1:Int,position2:Int){
    position+=((HeapArray(position1),position2))
    position+=((HeapArray(position2),position1))

    val tmp:T = HeapArray(position1)
    HeapArray(position1)=HeapArray(position2)
    HeapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(HeapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > getKey(HeapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(HeapArray(rightChild(position))) < getKey(HeapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > getKey(HeapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(HeapArray(position))
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && positionKey < getKey(HeapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Int):Int = (position+1)*2-1
  @inline
  private def rightChild(position:Int):Int =(position+1)*2
  @inline
  private def father(position:Int):Int = scala.math.floor((position+1)/2).toInt-1

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Int,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < size && getKey(HeapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),HeapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(HeapArray(0)),0,List.empty)
  }

  def getFirst:T=HeapArray(0)

  def removeFirst():T={
    val toreturn:T = HeapArray(0)
    swapPositions(0,size-1)
    size -=1
    position -= toreturn
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:T){
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T){
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position -= elem
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position -= elem
      pushDown(pushUp(startposition))
    }
  }
}

