package oscar.cp.linearizedDFS

/**
 * Created by saschavancauwelaert on 28/11/14.
 */

import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.algo.search._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore

sealed trait Decision extends Alternative {
  def name : String
  override def toString = name
}

object Decision {
  val push = "Push"
  val pop = "Pop"
  val assign = "=="
  val remove = "!="
  val fail = "Fail"
  val propagate = "Propagate"
}

sealed trait DomainDecision extends Decision {}
sealed trait ControlDecision extends Decision {}

case class Push(cp : DFSearchNode) extends ControlDecision {
  val name : String = Decision.push
  def apply() = {
    cp.pushState()
  }
}

case class Pop(cp : DFSearchNode) extends ControlDecision {
  val name : String = Decision.pop
  def apply() = {
    cp.pop()
  }
}

case class Assign(variable : CPIntVar, value : Int) extends DomainDecision {
  val name = variable.name + Decision.assign + value
  def apply() = {
    if(variable.assign(value) != Failure)
      variable.store.propagate()
    else
      variable.store.fail()
  }
}

case class Remove(variable : CPIntVar, value : Int) extends DomainDecision {
  val name = variable.name + Decision.remove + value
  def apply() = {
    if(variable.removeValue(value) != Failure)
      variable.store.propagate()
    else
      variable.store.fail()
  }
}

case class Fail(cp : CPStore) extends DomainDecision {
  val name = Decision.fail
  def apply() = cp.fail()
}

case class Propagate(cp : CPStore) extends DomainDecision {
  val name = Decision.propagate
  def apply() = cp.propagate()
}



/*
 * SetTimes
 */

abstract sealed class SetTimesBranch(protected val cp : CPStore, toBeReplayed : Boolean = false) extends DomainDecision {

  protected var replayStatus = false
  protected var failureStatus = false

  var behaviour : () => Unit = () => {
    normalBehaviour()
    if(toBeReplayed) {
      replayStatus = true
      if(cp.isFailed) {
        failureStatus = true
        behaviour = () => cp.fail()
      }
      else {
        behaviour = nonFailedReplayedBehaviour
      }
    }
  }

  def normalBehaviour() : Unit

  def nonFailedReplayedBehaviour() : Unit

  def apply() = {
    behaviour()
  }

}

case class SetTimesLeftBranch(starts : IndexedSeq[CPIntVar], est : Int, x : Int, bound : Array[ReversibleBoolean], updateAndCheck : () => Unit, toBeReplayed : Boolean = false) extends SetTimesBranch(starts(x).store, toBeReplayed) {

  val variable = starts(x)

  def name = if (replayStatus) (if (failureStatus) Decision.fail else starts(x).name + Decision.assign + est) else "normal"

  override def normalBehaviour = {
    if (variable.assign(est) != Failure)
      cp.propagate()
    else
      cp.fail()

    bound(x).value = true

    if (!cp.isFailed)
      updateAndCheck()
  }

  override def nonFailedReplayedBehaviour() = {
    if (variable.assign(est) != Failure)
      cp.propagate()
    else
      cp.fail()
  }

}

case class SetTimesRightBranch(store : CPStore, est : Int, x : Int, selectable : Array[ReversibleBoolean], oldEST : Array[ReversibleInt], updateAndCheck : () => Unit, toBeReplayed : Boolean = false) extends SetTimesBranch(store, toBeReplayed) {

  def name = if (replayStatus) (if (failureStatus) Decision.fail else Decision.propagate) else "normal"

  override def normalBehaviour = {
    cp.propagate()
    selectable(x).value = false
    oldEST(x).value = est
    updateAndCheck()
  }

  override def nonFailedReplayedBehaviour() = cp.propagate()

}



