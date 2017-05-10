package oscar.cp.searches.lns.selection

import scala.collection.mutable
import scala.util.Random

/**
 * Adaptive store backed by a priority queue.
 */
class PriorityStore[T](
                        e: Seq[T],
                        w: Seq[Double],
                        var rFactor: Double,
                        val min: Boolean
                      ) extends AdaptiveStore[T]{

  implicit val ordering: Ordering[(T, Double)] =
    if(min) Ordering.by(x => (x._2, Random.nextInt()))
    else Ordering.by(x => (-x._2, Random.nextInt()))

  val priority = new PriorityQueue[(T, Double)]
  (e zip w).foreach(priority += _)

  //Used to store the last selected elements which have not been adapted yet and are thus not in the PQ:
  var lastSelected = new mutable.HashSet[(T, Double)]()

  def this(elems: Seq[T], rFactor: Double, min: Boolean){
    this(elems, if(min) Array.fill(elems.length){0.0} else Array.fill(elems.length){Double.MaxValue}, rFactor, min)
  }

  override def select(): T = {

    //Adding the elements that have not been updated to the PQ
    if(lastSelected.nonEmpty){
      lastSelected.foreach(i => priority += i)
      lastSelected.clear()
    }

    //Selecting next elem
    val wrapper = priority.dequeue()
    lastSelected.add(wrapper)
    wrapper._1
  }

  override def adapt(elem: T, sFactor: Double, rFactor: Double): Unit = {
    getWrapper(elem) match{
      case None => throw new Exception("Element " + elem + " is not in store.")
      case Some((wrapper, cached: Boolean)) =>
        if(cached) lastSelected.remove(wrapper)
        else priority -= wrapper
        priority += ((wrapper._1, (1.0 - rFactor) * wrapper._2 + rFactor * sFactor))
    }
  }

  def getWrapper(elem: T): Option[((T, Double), Boolean)] = {
    lastSelected.foreach(x => if(x._1.equals(elem)) return Some(x, true))
    priority.foreach(x => if(x._1.equals(elem)) return Some(x, false))
    None
  }

  override def getElements: Seq[T] = (lastSelected.map(_._1) ++ priority.map(_._1)).toSeq

  override def remove(elem: T): Unit = {
    getWrapper(elem) match{
      case None => throw new Exception("Element " + elem + " is not in store.")
      case Some((wrapper, cached: Boolean)) =>
        if(cached) lastSelected.remove(wrapper)
        else priority -= wrapper
    }
  }

  override def add(elem: T, sFactor: Double): Unit = {
    priority += ((elem, sFactor))
  }

  override def nElements: Int = lastSelected.size + priority.size

  override def isEmpty: Boolean = lastSelected.isEmpty && priority.isEmpty

  override def nonEmpty: Boolean = !isEmpty
}
