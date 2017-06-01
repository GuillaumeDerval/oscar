package oscar.cp.searches.lns.selection

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * The purpose of the Roulette wheel is to select randomly an element based on adaptive probabilities.
 */
class RouletteWheel[T](
                        val elems: Array[T],
                        val weights: Array[Double],
                        var rFactor: Double,
                        val reversed: Boolean
                      ) extends AdaptiveStore[T]{

  private val lastSelected = mutable.HashMap[T, Int]() //caching for quick access to last selected elem(s)
  private val active = mutable.HashSet[Int](elems.indices: _*)

  def this(elems: Array[T], rFactor: Double, reversed:Boolean){this(elems, Array.fill(elems.length){1.0}, rFactor, reversed)}

  private def getIndex(elem: T): Int = lastSelected.getOrElse(elem, elems.indexOf(elem))

  private def isActive(index: Int): Boolean = active.contains(index)

  private def isCached(index: Int): Boolean = lastSelected.contains(elems(index))

  private def processProbas(activeSeq: Seq[Int]): Seq[Double] = {
    val activeWeights = activeSeq.map(weights(_))
    val wSum = activeWeights.sum

    //If all weights are at 0, giving equiprobability to all elems:
    if(wSum == 0.0) Seq.fill(activeSeq.length)(1.0/activeSeq.length)

    //If probabilities must be reversed, reversing weights:
    else if (reversed) {
      val reversedWeights = activeWeights.map(1.0 - _/wSum)
      val rwSum = reversedWeights.sum
      if(rwSum == 0.0) Seq.fill(activeSeq.length)(1.0/activeSeq.length)
      else reversedWeights.map(_  / rwSum)
    }

    else activeWeights.map(_ / wSum)
  }

  override def select(): T = {
    val rand = Random.nextDouble()
    val activeSeq = active.toSeq

    // Computing cumulative probabilities and returning first index for which it is greater than rand
    val selected = activeSeq(processProbas(activeSeq).scanLeft(0.0)(_+_).drop(1).indexWhere(_ > rand))
    lastSelected += elems(selected) -> selected
    elems(selected)
  }

  override def adapt(elem: T, sFactor: Double, rFactor: Double): Unit = {
    val index = getIndex(elem)
    if(index == -1) throw  new Exception("Element " + elem + " is not in store.")
    weights(index) = (1.0 - rFactor) * weights(index) + rFactor * sFactor
    if(isCached(index)) lastSelected.remove(elem)
  }

  override def getElements: Seq[T] = elems

  override def nElements: Int = elems.length

  override def getActive: Iterable[T] = active.map(elems(_))

  override def nActive: Int = active.size

  override def isActiveEmpty: Boolean = active.isEmpty

  override def nonActiveEmpty: Boolean = !isActiveEmpty

  override def deactivate(elem: T): Unit = {
    val index = getIndex(elem)
    if(index == -1) throw new Exception("Element " + elem + " is not in store.")
    active.remove(index)
    if(isCached(index)) lastSelected.remove(elem)
  }

  override def reset(sFactor: Double): Unit = {
    weights.indices.foreach(weights(_) = sFactor)
    reset()
  }

  override def reset(): Unit = {
    lastSelected.clear()
    active.clear()
    active ++= elems.indices
  }
}
