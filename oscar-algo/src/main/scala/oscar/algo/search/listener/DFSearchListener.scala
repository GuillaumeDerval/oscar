package oscar.algo.search.listener

import oscar.algo.search._

/**
  * Created by saschavancauwelaert on 23/02/16.
  */
trait DFSearchListener {

  // called on Push events
  def onPush(node : DFSearchNode) : Unit
  // called on Pop events
  def onPop(node : DFSearchNode) : Unit
  // called on branching
  def onBranch(node : DFSearchNode, alternative : Alternative) : Unit
}
