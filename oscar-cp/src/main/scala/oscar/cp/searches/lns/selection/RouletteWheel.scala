package oscar.cp.searches.lns.selection

import oscar.cp.searches.lns.operators.ALNSElement

import scala.collection.mutable
import scala.util.Random

/**
 * The purpose of the Roulette wheel is to select randomly an element based on adaptive probabilities.
 */
class RouletteWheel[T <: ALNSElement](
                        val elems: Array[T],
                        val reversed: Boolean
                      ) extends AdaptiveStore[T]{

  private val active = mutable.HashSet[Int](elems.indices: _*)

  private def isActive(index: Int): Boolean = active.contains(index)

  private def processProbas(activeSeq: Seq[Int]): Seq[Double] = {
    val activeWeights = activeSeq.map(elems(_).score)
    var wSum = activeWeights.sum
    if(wSum.isInfinity) wSum = Double.MaxValue - 1

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
    elems(selected)
  }

  override def adapt(elem: T): Unit = Unit

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
}
