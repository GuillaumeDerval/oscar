package oscar.cp.linearizedDFS

import oscar.algo.search._
import oscar.algo.search.listener.DFSearchListener

import scala.collection.mutable.ArrayBuffer

/**
  * Created by saschavancauwelaert on 23/02/16.
  */
class DFSLinearizer extends DFSearchListener {

  private[this] val searchStateModificationList_ : ArrayBuffer[Decision] = ArrayBuffer[Decision]()

  def searchStateModifications : Array[Decision] = searchStateModificationList_ toArray

  // called on Push events
  def onPush(node : DFSearchNode) : Unit = {
    searchStateModificationList_ += Push(node)
  }
  // called on Pop events
  def onPop(node : DFSearchNode) : Unit = {
    searchStateModificationList_ += Pop(node)
  }
  // called on branching
  def onBranch(alternative : Alternative) : Unit = {
    searchStateModificationList_ += alternative.asInstanceOf[Decision]
  }
}
