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

package oscar.cbls.algo.dll2

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
class DelayedPermaFilteredDoublyLinkedList[T <: AnyRef, F <: AnyRef] extends DoublyLinkedList[T]{

  /** this function is called on insert. It takes
    * -the inserted element,
    * -an insert function that performs the insert,
    * -a query function that can be called to check if the element is still in the list
    */
  private var mFilter:(T,()=>Unit, ()=> Boolean) => Unit = null
  private var mMap:T => F = null
  private var filtered:DoublyLinkedList[F] = null

  /**adds an a item in the PermaFilteredDLL, and if accepted by the filter, adds it in the slave PermaFilteredDLL.
    * returns a reference that should be used to remove the item from all those structures at once.
    */
  override def addElem(elem:T):DPFDLLStorageElement2[T,F] = {
    val d = new DPFDLLStorageElement2[T,F](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)
    if(mFilter != null) notifyInsert(d)
    d
  }

  @inline
  private def notifyInsert(inserted: DPFDLLStorageElement2[T,F]): Unit = {
    def injector():Unit = {inserted.filtered = filtered.addElem(mMap(inserted.elem))}
    def isStillValid():Boolean = {inserted.prev != null}
    mFilter(inserted.elem, injector, isStillValid)
  }

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
    var currentStorageElement:DLLStorageElement2[T]=phantom.next
    while(currentStorageElement!=phantom){
      notifyInsert(currentStorageElement.asInstanceOf[DPFDLLStorageElement2[T,F]])
      currentStorageElement = currentStorageElement.next
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param elem
 * @tparam T
 */
class DPFDLLStorageElement2[T,F](elem:T) extends DLLStorageElement2[T](elem){
  var filtered: DLLStorageElement2[F] = null

  override def delete(){
    super.delete()
    prev = null //this is checked by the delayed perma filter, so DO NOT REMOVE THIS SEEMIGNLY USELESS INSTRUCTION
    if(filtered != null) filtered.asInstanceOf[DLLStorageElement2[_]].delete()
  }
}
