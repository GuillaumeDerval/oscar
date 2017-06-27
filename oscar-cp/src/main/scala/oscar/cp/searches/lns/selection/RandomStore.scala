package oscar.cp.searches.lns.selection

import scala.collection.mutable
import scala.util.Random

/**
 * Fully random adaptive store.
 */
class RandomStore[T](val elems: Array[T]) extends AdaptiveStore[T]{
  var rFactor = 0.0

  private val active = mutable.HashSet[Int](elems.indices: _*)

  private def isActive(index: Int): Boolean = active.contains(index)

  override def select(): T = elems(active.toSeq(Random.nextInt(active.size)))

  override def adapt(elem: T, sFactor: Double, rFactor: Double): Unit = Unit

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

  override def reset(sFactor: Double): Unit = reset()

  override def reset(): Unit = {
    active.clear()
    active ++= elems.indices
  }
}
