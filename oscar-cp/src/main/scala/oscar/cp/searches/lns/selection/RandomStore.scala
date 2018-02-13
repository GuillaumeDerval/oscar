package oscar.cp.searches.lns.selection

import oscar.cp.searches.lns.operators.ALNSElement

import scala.collection.mutable
import scala.util.Random

/**
 * Fully random adaptive store.
 */
class RandomStore[T <: ALNSElement](val elems: Array[T]) extends AdaptiveStore[T]{
  var rFactor = 0.0

  private val active = mutable.HashSet[Int](elems.indices: _*)

  private def isActive(index: Int): Boolean = active.contains(index)

  override def select(): T = elems(active.toSeq(Random.nextInt(active.size)))

  override def adapt(elem: T): Double = 1.0 / nElements

  override def getElements: Seq[T] = elems

  override def nElements: Int = elems.length

  override def getActive: Iterable[T] = active.map(elems(_))

  override def nActive: Int = active.size

  override def isActiveEmpty: Boolean = active.isEmpty

  override def nonActiveEmpty: Boolean = !isActiveEmpty

  override def deactivate(elem: T): Unit = {
    val index = elems.indexOf(elem)
    if(index == -1) throw new Exception("Element " + elem + " is not in store.")
    active.remove(index)
  }

  override def reset(): Unit = {
    active.clear()
    active ++= elems.indices
  }

  override def getScore(elem: T): Double = 1.0 / nElements
}
