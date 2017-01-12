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

package oscar.cbls.algo.dll2

/** This is a mutable data structure allowing insert,
  * and delete in O(1) based on a key mechanism
  * @author renaud.delandtsheer@cetic.be
  * @tparam T
  */
class DoublyLinkedList[T] extends Iterable[T]{

  val phantom:DLLStorageElement2[T] = new DLLStorageElement2[T](null.asInstanceOf[T])

  phantom.setNext(phantom)

  /**returns the size of the DLL
    * this is a O(n) method because it is very rarely used.
    * and in this context, we want to keep the memory footprint as small as possible*/
  final override def size ={
    var toReturn = 0
    var current = phantom.next
    while(current != phantom){
      toReturn += 1
      current = current.next
    }
    toReturn
  }

  def addElem(elem:T):DLLStorageElement2[T] = {
    val d = new DLLStorageElement2[T](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)
    d
  }

  def +(elem:T){addElem(elem)}
  def ++(elems:Iterable[T]) {for(elem <- elems) addElem(elem)}

  def dropAll(){
    phantom.setNext(phantom)
  }

  override def isEmpty = phantom.next == phantom

  override def iterator = new DLLIterator[T](phantom, phantom)

  /**
   * @param fn the function to execute on each items included in this list
   * @tparam U the output type of the function
   * @return a list containing the result of executing fn on each element of the DLL. the list is in reverse order.
   */
  def mapToList[U](fn:T => U):List[U] = {
    val it = iterator
    var toReturn:List[U] = List.empty
    while(it.hasNext){
      toReturn = fn(it.next()) :: toReturn
    }
    toReturn
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param elem
 * @tparam T
 */
class DLLStorageElement2[T](val elem:T){
  var next:DLLStorageElement2[T] = null
  var prev:DLLStorageElement2[T] = null

  def setNext(d:DLLStorageElement2[T]){
    this.next = d
    d.prev = this
  }

  def delete(): Unit ={
    prev.setNext(next)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param CurrentKey
 * @param phantom
 * @tparam T
 */
class DLLIterator[T](var CurrentKey:DLLStorageElement2[T], val phantom:DLLStorageElement2[T]) extends Iterator[T]{
  def next():T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext:Boolean = {CurrentKey.next != phantom}
}
