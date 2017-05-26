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

  /**
    * Removes and returns the given element from the store.
    * @param elem The element to remove.
    */
  def remove(elem: T): Unit

  /**
    * adds the element to the store.
    * @param elem The element to add.
    * @param sFactor The initial selection factor value.
    */
  def add(elem: T, sFactor: Double): Unit

  def nElements: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean
}
