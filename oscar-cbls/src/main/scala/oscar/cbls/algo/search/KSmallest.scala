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

package oscar.cbls.algo.search

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList
import oscar.cbls.util.StopWatch

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
  def doSort(a:Array[Long],k:Long, key:Long => Long):List[Long] = {
    val heap = new BinomialHeap[Long](indice => -key(a(indice)),2L*k)
    for(i <- a.indices){
      heap.insert(i)
      if(i >= k)heap.popFirst()
    }
    heap.toList.map(a(_))
  }

  def doSortGetLater(a:Array[Long],key:Long => Long):KSmallest = new KSmallest(a,key)

  def lazySort(a:Array[Long], key:Long=>Long):Iterable[Long] = new LazyQuicksort(a,key)


  def kFirst(k: Long, values:Iterable[Long], filter: (Long => Boolean) = _ => true): Iterable[Long] = {

    def kFirstAccumulator(sortedNeighbors: Iterator[Long], k: Long): QList[Long] = {
      require(k >= 0L)
      if(k == 0L || !sortedNeighbors.hasNext){
        null
      }else{
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          QList(neighbor,kFirstAccumulator(sortedNeighbors, k - 1L))
        else
          kFirstAccumulator(sortedNeighbors, k)
      }
    }

    QList.toIterable(kFirstAccumulator(values.iterator, k))
  }
}

class KSmallest(a:Array[Long],key:Long => Long = a => a){
  //zipWithIndex puts index in second position of the couple
  val sorted:List[Long] = a.toList.zipWithIndex.sortBy(couple => key(couple._1)).map(_._2)

  def apply(k:Long):List[Long] = sorted.take(k)

  def apply(k:Long,filter:Long=>Boolean) = {
    def kSmallestAcc(sorted: List[Long], k: Long): List[Long] = {
      require(k >= 0L)
      if(k == 0L) return Nil
      sorted match {
        case Nil => Nil
        case h :: t =>
          if (filter(h)) h :: kSmallestAcc(t, k - 1L)
          else kSmallestAcc(t, k)
      }
    }
    kSmallestAcc(sorted, k)
  }
}

object testQuickSort extends App with StopWatch{

  val n = 10000000L
  val k = 500L
  val randomValues = Array.tabulate(n)(_ => (math.random * Long.MaxValue).toInt)

  startWatch()
  val s = KSmallest.doSort(randomValues,k,x => x)
  val watch1 = getWatch

  startWatch()
  val qs = new LazyQuicksort(randomValues)
  val it = qs.iterator
  for(i <- 1L to k){
    val j = it.next()
  }
  val watch2 = getWatch

  println("nonLazy:" + watch1)
  println("lazy:" + watch2)
}

/**
 * this implementation will perform a lazy sort.
 * it will sort on demand, as required by the iterator, or by an explicit call to sortUntil
 * @param array an array containing the values to sort. the array will be modified by this procedure, to clone it if you need it somewhere else!
 */
class LazyQuicksort(val array:Array[Long], key:Long => Long = a => a) extends Iterable[Long] {

  class QList(val left:Long, val right:Long, val tail:QList)
  private[this] var toDo: QList = new QList(0L, array.length - 1L,null)

  private[this] var lastSortedPosition = -1L
  def sortUntil(k: Long) {
    if(k <= lastSortedPosition) return
    while (true) {
      if (toDo == null) return
      val l = toDo.left
      if(l <= k){
        val r = toDo.right
        toDo = toDo.tail
        sort1(l, r)
      }else {
        lastSortedPosition = l - 1L
        return;
      }
    }
  }

  @inline
  private[this] def sort1(l: Long, r: Long) {
    val pivot: Long = key(array((l + r) / 2L))
    var i = l
    var j = r
    while (i <= j) {
      while (key(array(i)) < pivot) i += 1L
      while (key(array(j)) > pivot) j -= 1L
      //we know that array(i) >= pivot && array(j) <= pivot
      if (i <= j) {
        val t = array(i)
        array(i) = array(j)
        array(j) = t
        i += 1L
        j -= 1L
      }
    }
    if (i < r) toDo = new QList(i, r,toDo)
    if (l < j) sort1(l, j)
    else lastSortedPosition = j //this is an incomplete update, but this is an approximate value, so we do not care too much.
  }

  override def iterator: Iterator[Long] = new LazyQuickSortIterator(this)

  class LazyQuickSortIterator(l:LazyQuicksort) extends Iterator[Long]{
    var nextPos:Long = 0L
    override val length = l.array.length

    override def hasNext: Boolean = {
      nextPos < length
    }

    override def next(): Long = {
      l.sortUntil(nextPos)
      val toReturn = l.array(nextPos)
      nextPos += 1L
      toReturn
    }
  }
}
