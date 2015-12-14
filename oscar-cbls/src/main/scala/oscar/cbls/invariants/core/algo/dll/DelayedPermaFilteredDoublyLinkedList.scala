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
  *            Gaël Thouvenin
  ******************************************************************************/

package oscar.cbls.invariants.core.algo.dll

/**this is a mutable data structure that is able to represent sets through doubly-lined lists, with insert
  * and delete in O(1) through reference
  * and to update in parallel another set that is a filter of the first one through a specified function
  * the filter can be specified anytime and filtering can be cascaded, but a PermaFilteresDLL can have only one filter
  *
  * You should not perform any operation on the slave DLL,
  * although this will not be detected and reported as an error
  *
  * Beware that this is a mutable data structure, hence you should not perform any update on it while iterating on it.
  * @author renaud.delandtsheer@cetic.be
  * @author gael.thouvenin@student.umons.ac.be
  * */
class DelayedPermaFilteredDoublyLinkedList[T <: AnyRef, F <: AnyRef] extends Iterable[T]{

  private[this] val phantom:DPFDLLStorageElement[T] = new DPFDLLStorageElement[T](null.asInstanceOf[T])
  phantom.setNext(phantom)

  /** this function is called on insert. It takes
    * -the inserted element,
    * -an insert function that performs the insert,
    * -a query function that can be called to check if the element is still in the list
    */
  private[this] var mFilter:(T,()=>Unit, ()=> Boolean) => Unit = null
  private[this] var mMap:T => F = null
  private[this] var filtered:DoublyLinkedList[F] = null

  def headPhantom = phantom

  /**returns the size of the PermaFilteredDLL
    * this is a O(n) method because it is very rarely used.
    * and in this context, we want to keep the memory footprint as small as possible*/
  override def size ={
    var toReturn = 0
    var current = phantom.next
    while(current != phantom){
      toReturn += 1
      current = current.next
    }
    toReturn
  }

  /**adds an a item in the PermaFilteredDLL, and if accepted by the filter, adds it in the slave PermaFilteredDLL.
    * returns a reference that should be used to remove the item from all those structures at once.
    */
  def addElem(elem:T):DPFDLLStorageElement[T] = {
    val d = new DPFDLLStorageElement[T](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)

    if(mFilter != null) notifyInsert(d)

    d
  }

  /**adds an element to the data structure, cfr. method addElem*/
  def +(elem:T){addElem(elem)}

  /**adds a bunch of items to the data structures*/
  def ++(elems:Iterable[T]) {for(elem <- elems) addElem(elem)}

  @inline
  private def notifyInsert(inserted: DPFDLLStorageElement[T]): Unit = {
    def injector():Unit = {inserted.filtered = filtered.addElem(mMap(inserted.elem))}
    def isStillValid():Boolean = {inserted.prev != null}
    mFilter(inserted.elem, injector, isStillValid)
  }

  override def isEmpty:Boolean = phantom.next == phantom

  override def iterator = new DPFDLLIterator[T](phantom,phantom)

  def delayedPermaFilter(filter:(T,()=>Unit, ()=> Boolean) => Unit,
                                      mMap:T => F = (t:T) => t.asInstanceOf[F]):DoublyLinkedList[F] = {
    assert(mFilter == null,"DelayedPermaFilteredDoublyLinkedList can only accept a single filter")

    val filtered = new DoublyLinkedList[F]

    mFilter = filter
    this.mMap = mMap
    this.filtered = filtered
    filterElementsForNewFilter()

    filtered
  }

  private def filterElementsForNewFilter(){
    var currentstorageElement:DPFDLLStorageElement[T]=phantom.next
    while(currentstorageElement!=phantom){
      notifyInsert(currentstorageElement)
      currentstorageElement = currentstorageElement.next
    }
  }

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

  override def foreach[U](f: (T) => U): Unit = {
    var currentPos = headPhantom.next
    while(currentPos != headPhantom){
      f(currentPos.elem)
      currentPos = currentPos.next
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param elem
 * @tparam T
 */
class DPFDLLStorageElement[T](val elem:T){
  var next:DPFDLLStorageElement[T] = null
  var prev:DPFDLLStorageElement[T] = null
  var filtered: AnyRef = null

  def setNext(d:DPFDLLStorageElement[T]){
    this.next = d
    d.prev = this
  }

  def delete(): Unit =
  {
    prev.setNext(next)
    prev = null //this is checked by the delayed perma filter, so DO NOT REMOVE THIS SEEMIGNLY USELESS INSTRUCTION
    if(filtered != null) filtered.asInstanceOf[DLLStorageElement[_]].delete()
  }
}

class DPFDLLIterator[T](var CurrentKey:DPFDLLStorageElement[T],
                       val phantom:DPFDLLStorageElement[T]) extends Iterator[T]{
  def next():T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext:Boolean = {CurrentKey.next != phantom}
}
