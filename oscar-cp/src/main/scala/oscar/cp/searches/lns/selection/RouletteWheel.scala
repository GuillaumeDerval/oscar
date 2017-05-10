package oscar.cp.searches.lns.selection

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * The purpose of the Roulette wheel is to select randomly an element based on adaptive probabilities.
 */
class RouletteWheel[T](
                        e: Seq[T],
                        w: Seq[Double],
                        var rFactor: Double,
                        val reversed: Boolean
                      ) extends AdaptiveStore[T]{

  private val elems = ArrayBuffer[T](e: _*) //Elements
  private val weights = ArrayBuffer[Double](w: _*) //Weights
  private var lastSelected: mutable.Set[Int] = mutable.HashSet() //caching for quick access to last selected elem(s)

  def this(elems: Seq[T], rFactor: Double, reversed:Boolean){this(elems, Array.fill(elems.size){1.0}, rFactor, reversed)}

  private def getIndex(elem: T): Int = {
    lastSelected.foreach(i => if(elems(i).equals(elem)) return i)
    elems.indexOf(elem)
  }

  override def select(): T = {
    // random between 0 and 1
    val rand = Random.nextFloat()

    // Computing cumulative probabilities and returning first index for which it is greater than rand
    val selected = processProbas.scanLeft(0.0)(_+_).drop(1).indexWhere(_ > rand)
    lastSelected += selected
    elems(selected)
  }

  override def adapt(elem: T, sFactor: Double, rFactor: Double): Unit = {
    val i = getIndex(elem)
    if(i == -1) throw  new Exception("Element " + elem + " is not in store.")
    weights(i) = (1.0 - rFactor) * weights(i) + rFactor * sFactor
    lastSelected.remove(i)
  }

  def processProbas: Seq[Double] = {
    val wSum = weights.sum

    //If all weights are at 0, giving equiprobability to all elems:
    if(wSum == 0.0) weights.map(_ => 1.0/weights.length)

    //If probabilities must be reversed, reversing weights:
    else if (reversed) {
      val reversedWeights = weights.map(1.0 - _/wSum)
      val rwSum = reversedWeights.sum
      if(rwSum == 0.0) reversedWeights.map(_ => 1.0/reversedWeights.length)
      else reversedWeights.map(_ / rwSum)
    }

    else weights.map(_ / wSum)
  }

  override def getElements: Seq[T] = elems

  override def add(elem: T, sFactor: Double): Unit = {
    if(getIndex(elem) != -1) throw new Exception("Element " + elem + " is already in store.")
    elems += elem
    weights += sFactor
  }

  override def remove(elem: T): Unit = {
    val i = getIndex(elem)
    if(i != -1){
      elems.remove(i)
      weights.remove(i)
      lastSelected.clear()
    }
  }

  override def nElements: Int = elems.length

  override def isEmpty: Boolean = elems.isEmpty

  override def nonEmpty: Boolean = !isEmpty
}
