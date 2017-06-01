package oscar.cp.searches.lns.selection

/**
 * This trait defines an adaptive store. it is defined by two operations: select which returns a element based on
  * an internal mechanism and adapt which allows to adapt the selection factor of an element.
 */
trait AdaptiveStore[T]{

  var rFactor: Double //reaction factor: used when adapting a selection factor to balance the old and the new value.

  /**
    * Selects and returns an element based on the internal selection mechanism.
    */
  def select(): T

  /**
    * Adapts the selection factor for a given element
    * @param elem The element for which the selection factor must be adapted
    * @param sFactor The new value for the selection factor
    * @param rFactor The reaction factor balancing the new and old values of the selection factor
    */
  def adapt(elem: T, sFactor: Double, rFactor: Double = this.rFactor): Unit

  /**
    * Returns all the elements contained in the store.
    * @return an iterable of all the elements in the store.
    */
  def getElements: Iterable[T]

  def nElements: Int

  /**
    * Returns all the active elements contained in the store.
    * @return an iterable of all the active elements in the store.
    */
  def getActive: Iterable[T]

  def nActive: Int

  def isActiveEmpty: Boolean

  def nonActiveEmpty: Boolean

  /**
    * deactivates the given element from the store.
    * @param elem The element to remove.
    */
  def deactivate(elem: T): Unit

  /**
    * Resets all elements of the store to the given factor.
    * @param sFactor The selection factor value to give to all elements.
    */
  def reset(sFactor: Double): Unit

  /**
    * Resets all elements of the store.
    */
  def reset(): Unit
}
