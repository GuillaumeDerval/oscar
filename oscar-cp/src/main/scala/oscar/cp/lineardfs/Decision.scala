package oscar.cp.lineardfs

import oscar.algo.search._

/**
 * Created by saschavancauwelaert on 28/11/14.
 */

sealed trait Decision extends Alternative {}
sealed trait ControlDecision extends Decision {}

class Push(cp : DFSearchNode) extends ControlDecision {
  def apply() = {
    cp.pushState()
  }
}

class Pop(cp : DFSearchNode) extends ControlDecision {
  def apply() = {
    cp.pop()
  }
}

class AlternativeDecision(alternative: Alternative) extends Decision {
  def apply() = alternative.apply()
}