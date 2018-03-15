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

package oscar.cbls.search.algo

import oscar.cbls.invariants.core.algo.heap.BinomialHeap
import oscar.cbls.search.StopWatch

/**
 * This class serves to compute the k-smallest values of a given vector.
 * this computation can be done either one-shot, or with gradually increasing k
 */
object KSmallest {

  /**
   * returns the k smallest elements, but they are not sorted between themselves.
   * @param a
   * @param k
   * @param key
   * @return
   */
  def doSort(a:Array[Int],k:Int, key:Int => Int):List[Int] = {
    val heap = new BinomialHeap[Int](indice => -key(a(indice)),2*k)
    for(i <- a.indices){
      heap.insert(i)
      if(i >= k)heap.popFirst()
    }
    heap.toList.map(a(_))
  }

  def doSortGetLater(a:Array[Int],key:Int => Int):KSmallest = new KSmallest(a,key)

  def lazySort(a:Array[Int], key:Int=>Int):Iterable[Int] = new LazyQuicksort(a,key)
}

class KSmallest(a:Array[Int],key:Int => Int = a => a){
  //zipWithIndex puts index in second position of the couple
  val sorted:List[Int] = a.toList.zipWithIndex.sortBy(couple => key(couple._1)).map(_._2)

  def apply(k:Int):List[Int] = sorted.take(k)

  def apply(k:Int,filter:Int=>Boolean) = {
    def kSmallestAcc(sorted: List[Int], k: Int): List[Int] = {
      require(k >= 0)
      if(k == 0) return Nil
      sorted match {
        case Nil => Nil
        case h :: t =>
          if (filter(h)) h :: kSmallestAcc(t, k - 1)
          else kSmallestAcc(t, k)
      }
    }
    kSmallestAcc(sorted, k)
  }
}

object testQuickSort extends App with StopWatch{

  val n = 10000000
  val k = 500
  val randomValues = Array.tabulate(n)(_ => (math.random * Int.MaxValue).toInt)

  startWatch()
  val s = KSmallest.doSort(randomValues,k,x => x)
  val watch1 = getWatch

  startWatch()
  val qs = new LazyQuicksort(randomValues)
  val it = qs.iterator
  for(i <- 1 to k){
    val j = it.next()
  }
  val watch2 = getWatch

  println("nonLazy:" + watch1)
  println("lazy:" + watch2)
}

/**
 * this implemetnation will perform a lazy sort.
 * it will sort on demand, as required by the iterator, or by an explicit call to sortUntil
 * @param array
 */
class LazyQuicksort(val array:Array[Int], key:Int => Int = a => a) extends Iterable[Int] {

  class QList(val left:Int, val right:Int, val tail:QList)
  private[this] var toDo: QList = new QList(0, array.length - 1,null)

  private[this] var lastSortedPosition = -1
  def sortUntil(k: Int) {
    if(k <= lastSortedPosition) return
    while (true) {
      if (toDo == null) return
      val l = toDo.left
      if(l <= k){
        val r = toDo.right
        toDo = toDo.tail
        sort1(l, r)
      }else {
        lastSortedPosition = l - 1
        return;
      }
    }
  }

  @inline
  private[this] def sort1(l: Int, r: Int) {
    val pivot: Int = key(array((l + r) / 2))
    var i = l
    var j = r
    while (i <= j) {
      while (key(array(i)) < pivot) i += 1
      while (key(array(j)) > pivot) j -= 1
      //we know that array(i) >= pivot && array(j) <= pivot
      if (i <= j) {
        val t = array(i)
        array(i) = array(j)
        array(j) = t
        i += 1
        j -= 1
      }
    }
    if (i < r) toDo = new QList(i, r,toDo)
    if (l < j) sort1(l, j)
    else lastSortedPosition = j //this is an incomplete update, but this is an approximate value, so we do not care too much.
  }

  override def iterator: Iterator[Int] = new LazyQuickSortIterator(this)

  class LazyQuickSortIterator(l:LazyQuicksort) extends Iterator[Int]{
    var nextPos:Int = 0
    override val length = l.array.length

    override def hasNext: Boolean = {
      nextPos < length
    }

    override def next(): Int = {
      l.sortUntil(nextPos)
      val toReturn = l.array(nextPos)
      nextPos += 1
      toReturn
    }
  }
}

class NextIterator[T](base:Iterator[T]) extends Iterator[T]{
  var nextToReturn:T = null.asInstanceOf[T]
  var anyNextToReturn:Boolean = false

  override def hasNext(): Boolean = anyNextToReturn || base.hasNext

  override def next(): T = {
    if (anyNextToReturn){
      anyNextToReturn = false
      nextToReturn
    } else{
      base.next()
    }
  }

  def pushBack(t:T){
    require(!anyNextToReturn,"can only push back one element")
    anyNextToReturn = true
    nextToReturn = t
  }
}

class LazyMap[T](over:Iterable[Int],map:Int => T) extends Iterable[T]{
  override def iterator: Iterator[T] = new LazyMapIterator(over.iterator,map)
}

class LazyMapIterator[T](over:Iterator[Int],map:Int => T) extends Iterator[T] {
  override def hasNext: Boolean = over.hasNext
  override def next(): T = map(over.next())
}

class LazyFilter(over:Iterable[Int],filter:Int => Boolean) extends Iterable[Int]{
  override def iterator: Iterator[Int] = new LazyFilteredIterator(over.iterator,filter)
}

class LazyFilteredIterator(over:Iterator[Int],filter:Int => Boolean) extends Iterator[Int] {
  val overPB = new NextIterator[Int](over)
  var nextExistsAndIsFiltered = false

  override def hasNext: Boolean = {
    prepareNextTrueIfExists()
  }

  private[this] def prepareNextTrueIfExists(): Boolean = {
    if(nextExistsAndIsFiltered) return true
    while (overPB.hasNext()) {
      val potentialNext = overPB.next()
      if (filter(potentialNext)) {
        overPB.pushBack(potentialNext)
        return true
      }
    }
    false
  }

  override def next(): Int = {
    require(prepareNextTrueIfExists(),"next does nt exist")
    nextExistsAndIsFiltered = false
    overPB.next()
  }
}
