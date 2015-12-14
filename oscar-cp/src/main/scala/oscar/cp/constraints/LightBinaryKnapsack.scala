package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.algo.reversible.ReversibleBoolean
import oscar.cp.core.watcher.Watcher
import oscar.algo.SortUtils

/** @author Renaud Hartert ren.hartert@gmail.com */

final class WatcherKnapsack(knapsack: Constraint, boolVar: CPBoolVar, weight: Int, requiredLoad: ReversibleInt, possibleLoad: ReversibleInt) extends Watcher {
  private[this] val store = boolVar.store
  final override def awake(): Unit = {
    if (!knapsack.inPropagate()) {
      if (boolVar.isTrue) requiredLoad += weight
      else possibleLoad -= weight
      store.enqueueL2(knapsack)
    }
  }
}

final class LightBinaryKnapsack(items: Array[CPBoolVar], weights: Array[Int], load: CPIntVar) extends Constraint(load.store, "LightBinaryKnapsack") {

  idempotent = true

  private[this] val nItems = items.length
  private[this] val sortedItems = Array.tabulate(nItems)(i => i)
  private[this] val largestItemRev = new ReversibleInt(s, 0)
  private[this] var largestItem = 0

  private[this] val requiredLoadRev = new ReversibleInt(s, 0)
  private[this] val possibleLoadRev = new ReversibleInt(s, 0)
  private[this] var requiredLoad = 0
  private[this] var possibleLoad = 0

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (init() == Failure) Failure
    else {
      var i = nItems
      while (i > 0) {
        i -= 1
        val watcher = new WatcherKnapsack(this, items(i), weights(i), requiredLoadRev, possibleLoadRev)
        items(i).awakeOnChanges(watcher)
      }
      load.callPropagateWhenDomainChanges(this)
      Suspend
    }
  }

  @inline private def init(): CPOutcome = {
    // Reset structures
    requiredLoad = 0
    possibleLoad = 0
    // Sort items
    SortUtils.mergeSort(sortedItems, weights)
    // Compute loads
    var i = nItems
    while (i > 0) {
      i -= 1
      val item = items(i)
      if (!item.isBound) possibleLoad += weights(i)
      else if (item.isTrue) {
        val weight = weights(i)
        requiredLoad += weight
        possibleLoad += weight
      }
    }
    // Largest 
    largestItem = nItems - 1
    while (largestItem >= 0 && items(sortedItems(largestItem)).isBound) largestItem -= 1
    // Initial filtering
    if (filterItems == Failure) Failure
    else {
      largestItemRev.value = largestItem
      requiredLoadRev.value = requiredLoad
      possibleLoadRev.value = possibleLoad
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    // Cache
    largestItem = largestItemRev.value
    requiredLoad = requiredLoadRev.value
    possibleLoad = possibleLoadRev.value
    // Filtering
    if (filterItems() == Failure) Failure
    else {
      // Trail
      largestItemRev.value = largestItem
      requiredLoadRev.value = requiredLoad
      possibleLoadRev.value = possibleLoad
      Suspend
    }
  }

  @inline private def filterItems(): CPOutcome = {
    if (load.updateMax(possibleLoad) == Failure) return Failure
    else if (load.updateMin(requiredLoad) == Failure) return Failure
    else {
      var maxWeight = load.max - requiredLoad
      var minWeight = possibleLoad - load.min
      var continue = largestItem >= 0
      while (continue && largestItem >= 0) {
        val itemId = sortedItems(largestItem)
        val item = items(itemId)
        if (item.isBound) largestItem -= 1
        else {
          val weight = weights(itemId)
          if (weight > maxWeight) {
            if (item.assignFalse() == Failure) return Failure
            else {
              possibleLoad -= weight
              if (load.updateMax(possibleLoad) == Failure) return Failure
              else {
                largestItem -= 1
                minWeight -= weight
                maxWeight = load.max - requiredLoad
              }
            }
          } else if (minWeight < weight) {
            if (item.assignTrue() == Failure) return Failure
            else {
              requiredLoad += weight
              if (load.updateMin(requiredLoad) == Failure) return Failure
              else {
                largestItem -= 1
                minWeight = possibleLoad - load.min
                maxWeight -= weight
              }
            }
          } else continue = false
        }
      }
    }
    Suspend
  }
}