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
  *           Gaël Thouvenin
 ******************************************************************************/


package oscar.cbls.algo.set

import scala.collection.Iterator

/**Circular set is a dedicated data structure to represent set of integers in an efficient way
 * although the memory footprint will not be efficient at all
 * stores elements from 0L to size-1L
 * ensure that the maxsize is not so big because it consumes O(maxsize) memory.
  * @author renaud.delandtsheer@cetic.be
  * @author gael.thouvenin@student.umons.ac.be
 */
class CircularIntSet(val maxsize:Long) extends scala.collection.mutable.SortedSet[Long]{

  private val containsvar:Array[Boolean] = new Array[Boolean](maxsize)
  private[set] val next:Array[Long] = new Array[Long](maxsize) //gives the id of the next element, so that they constitute a cycle in the array
  private[set] val prev:Array[Long] = new Array[Long](maxsize) //gives the id of the next element, so that they constitute a cycle in the array

  private var handle:Long = -1L
  private var sizevar:Long = 0L
  private var inserted:Boolean = false

  def -=(elem: Long):this.type = {
    if(containsvar(elem)) {
      containsvar(elem) = false
      if (handle == elem) {
        if (sizevar == 1L) {
          handle = -1L
          next(elem) = -1L
          prev(elem) = -1L
          sizevar = 0L
          return this
        } else {
          handle = next(elem)
        }
      }

      next(prev(elem)) = next(elem)
      prev(next(elem)) = prev(elem)
      next(elem) = -1L
      prev(elem) = -1L
      sizevar -= 1L
    }
    this
  }

  def +=(elem: Long):this.type = {
    if (sizevar == 0L) {
      containsvar(elem) = true
      next(elem) = elem
      prev(elem) = elem
      sizevar = 1L
      handle = elem
      inserted = true
    }else if (!containsvar(elem)) {
      containsvar(elem) = true
      insertAfter(handle, elem)
      sizevar += 1L
      inserted = true
    }
  this
  }

  private def insertAfter(elem:Long, newElem:Long){
    val elemAfter:Long = next(elem)
    next(elem) = newElem
    next(newElem) = elemAfter
    prev(elemAfter) = newElem
    prev(newElem) = elem
  }

  /**
   *  Orders the collection. Should be called only if an element has been inserted since last call,
   *  i.e. if inserted == true
   */
  private def reorder():Unit =
  {
    val values = for(e <- this) yield e
    val sortedValues = values.toList.sorted
    sortedValues match {
      case _ :: _ :: _ =>
        for (i <- 1L until sortedValues.size - 1L) {
          next(sortedValues(i)) = sortedValues(i + 1L)
          prev(sortedValues(i)) = sortedValues(i - 1L)
        }
        prev(sortedValues.head) = sortedValues.last
        next(sortedValues.head) = sortedValues(1L)
        prev(sortedValues.last) = sortedValues(sortedValues.size - 2L)
        next(sortedValues.last) = sortedValues.head
        handle = sortedValues.head

      case _ if size > 0L => handle = sortedValues.head
    }
    inserted = false
  }

  override def size: Int = sizevar

  def contains(elem: Long): Boolean = this.containsvar(elem)

  /**
   * Returns a NON-NECESSARILY ordered iterator over the set
   * @return
   */
  def iterator: Iterator[Long] = new CircularIntSetIterator(handle, this)

  override implicit def ordering: Ordering[Long] = Ordering.Long

  /**
   * Creates and returns a sorted version of this set
   * @param from lower bound (inclusive) of the elements to keep
   * @param until upper bound (inclusive) of the elements to keep
   * @return sorted version of this set, truncated such as each element \in [from, until]
   */
  override def rangeImpl(from: Option[Long], until: Option[Long]): scala.collection.mutable.SortedSet[Long] = {
    if(inserted) reorder()
    new CircularIntSet(maxsize) ++ (for(e <- this if e >= from.getOrElse(0L) && e <= until.getOrElse(maxsize)) yield e)
  }

  /**
   * Returns a sorted iterator over the set such as each element has a value over start
   * @param start lower bound (inclusive) of the elements to keep
   * @return an iterator over the elements > start
   */
  override def keysIteratorFrom(start: Long): Iterator[Long] =
  {
    if(inserted) reorder()
    new CircularIntSetIterator(handle, this).filter(x => x >= start)
  }
}

class CircularIntSetIterator(handle:Long, on:CircularIntSet) extends Iterator[Long]{
  var current = handle
  var initposition:Boolean = true

  def hasNext: Boolean = on.size > 0L & ( initposition || current != handle )

  def next(): Long = {
    initposition = false
    current = on.next(current)
    on.prev(current)
  }
}

