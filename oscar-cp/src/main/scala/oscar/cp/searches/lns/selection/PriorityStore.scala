package oscar.cp.searches.lns.selection

import java.util.PriorityQueue

import oscar.cp.searches.lns.operators.ALNSElement

import scala.collection.mutable
import scala.util.Random
import scala.collection.JavaConversions._

/**
 * Adaptive store backed by a priority queue.
 */
class PriorityStore[T <: ALNSElement](
                        val elems: Array[T],
                        val min: Boolean
                      ) extends AdaptiveStore[T]{

  private val ordering: Ordering[Int] =
    if(min) Ordering.by(x => (elems(x).score, Random.nextInt()))
    else Ordering.by(x => (-elems(x).score, Random.nextInt()))

  private val priority = new PriorityQueue[Int](elems.length, ordering)
  elems.indices.foreach(priority.add)

  //Used to store the last selected elements which have not been adapted yet and are thus not in the PQ:
  private val lastSelected = new mutable.HashMap[T, Int]()
  private val deactivated = new mutable.HashSet[Int]()

  private def getIndex(elem: T): Int = lastSelected.getOrElse(elem, elems.indexOf(elem))

  private def isCached(index: Int): Boolean = lastSelected.contains(elems(index))

  private def isActive(index: Int): Boolean = !deactivated.contains(index)

  override def select(): T = {
    if(isActiveEmpty) throw new Exception("There is no active element in the store.")

    //Adding the elements that have not been updated to the PQ
    if(lastSelected.nonEmpty){
      lastSelected.values.foreach(priority.add)
      lastSelected.clear()
    }

    //Selecting next elem
    val index = priority.poll()
    lastSelected += elems(index) -> index
    elems(index)
  }

  override def adapt(elem: T): Unit = {
    val index = getIndex(elem)
    if(index == -1) throw new Exception("Element " + elem + " is not in store.")
    if(isCached(index)) lastSelected.remove(elem)
    else priority.remove(index)
    priority.add(index)
  }

  override def getElements: Seq[T] = elems

  override def nElements: Int = elems.length

  override def getActive: Iterable[T] = (lastSelected.values ++ priority.toSeq).map(elems(_))

  override def nActive: Int = elems.length - deactivated.size

  override def isActiveEmpty: Boolean = lastSelected.isEmpty && priority.isEmpty

  override def nonActiveEmpty: Boolean = !isActiveEmpty

  override def deactivate(elem: T): Unit = {
    val index = getIndex(elem)
    if(index == -1) throw new Exception("Element " + elem + " is not in store.")
    if(isCached(index)) lastSelected.remove(elem)
    else priority.remove(index)
    deactivated += index
  }

  override def reset(): Unit = {
    priority.clear()
    lastSelected.clear()
    deactivated.clear()
    elems.indices.foreach(priority.add)
  }
}
