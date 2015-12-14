package oscar.sat.heuristics

import oscar.algo.array.ArrayHeapDouble
import oscar.sat.core.CDCLStore

final class ActivityHeuristic(nVariables: Int, store: CDCLStore) extends Heuristic {
  
  private[this] val heap = new ArrayHeapDouble(nVariables)
  private[this] val inHeap = new Array[Boolean](nVariables)
  private[this] val activities = new Array[Double](nVariables)

  override def init(): Unit = {
    heap.clear()
    var i = nVariables
    while (i > 0) {
      i -= 1
      if (store.isAssigned(i)) inHeap(i) = false
      else {
        val act = -store.varActivity(i)
        activities(i) = act
        inHeap(i) = true
        heap.enqueue(act, i)
      }
    }
  }

  override def nextLiteral(): Int = {
    var varId = heap.dequeue()
    inHeap(varId) = false
    while (store.isAssigned(varId)) {
      varId = heap.dequeue()
      inHeap(varId) = false
    }
    varId * 2
  }

  override def undo(varId: Int): Unit = {
    if (inHeap(varId)) updateActivity(varId)
    else {
      inHeap(varId) = true
      val act = -store.varActivity(varId)
      activities(varId) = act
      heap.enqueue(act, varId)
    }
  }

  override def updateActivity(varId: Int): Unit = {
    val newAct = -store.varActivity(varId)
    if (inHeap(varId)) {
      val oldAct = activities(varId)
      heap.changeKey(oldAct, newAct, varId)
    }
    activities(varId) = newAct
  }
}