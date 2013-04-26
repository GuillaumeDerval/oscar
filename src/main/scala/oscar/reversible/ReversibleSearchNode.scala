/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.reversible;

import java.util.Random
import java.util.Stack
import scala.util.continuations._
import oscar.search.DummyObjective
import oscar.search.Objective
import oscar.search._
import oscar.search.Tree

/**
 * Class representing a reversible search node, that is a node able to restore all
 * the reversible state attached to it (see Reversibles). <br>
 * A reversible search node is used to find solution by exploration of search tree (see Search).
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSearchNode {

  var silent = false

  var magic = 0
  var trail = new Trail();
  val pointerStack = new Stack[TrailEntry]()

  val random: Random = new Random(0)
  val failed = new ReversibleBool(this, false)

  var sc: SearchController = new DFSSearchController(this)

  /**
   * time (ms) spend in last exploration
   */
  var time: Long = 0
  /**
   * number of backtracks in last exploration
   */
  var bkts: Int = 0

  private var limit = Int.MaxValue
  private var tLimit = Int.MaxValue

  // tree visu
  val tree = new Tree(false)
  var currParent = 0
  var nodeMagic = 0
  def recordTree() {
    tree.clear()
    tree.record = true
  }

  def failLimit = limit
  def failLimit_=(lim: Int) {
    sc.failLimit_=(lim)
    limit = lim
  }

  def timeLimit = tLimit
  def timeLimit_=(lim: Int) {
    sc.timeLimit_=(lim)
    tLimit = lim
  }

  /**
   *
   * @return The Random generator of this node potentially used in other algorithms
   */
  def getRandom() = random

  /**
   *
   * @return  true if this node can surely not lead to any solution
   */
  def isFailed(): Boolean = failed.value

  /**
   * Set the node in a failed state
   */
  def fail() {
    failed.setValue(true)
  }

  /**
   * @return the number of fail
   */
  def nFail() = sc.nFail()

  /**
   * Exit the search in progress and/or the LNS if any
   */
  def stop() {
    sc.stop()
  }

  def getMagic() = magic

  def getTrail() = trail

  def solFound() = {
    tree.addSuccess(currParent)
  }

  /**
   * Store the current state of the node on a stack.
   */
  def pushState() {
    magic += 1
    pointerStack.push(trail.getTopEntry())
  }

  /**
   * Restore state on top of the stack of states and remove it from the stack.
   */
  def pop() {
    trail.restoreUntil(pointerStack.pop())
    magic += 1 // increment the magic because we want to trail again
  }

  /**
   * Restore the node to its initial state
   */
  def popAll() {
    while (!pointerStack.empty()) {
      trail.restoreUntil(pointerStack.pop())
    }
    magic += 1 // increment the magic because we want to trail again
  }

  override def toString() = {
    "ReversibleSearchNode: nbPushed" + pointerStack.size() + " currentTrailSize:" + trail.getSize();
  }


  /**
   * executed just before the actual branch action
   */
  def beforeBranch() = {}

  /**
   * executed just after the actual branch action
   */
  def afterBranch() = {}

  def branch(left: => Unit, leftLabel:String = "")(right: => Unit, rightLabel:String = "") = {
    val idleft = nodeMagic + 1
    val idright = nodeMagic + 2
    nodeMagic += 2
    tree.addBranch(currParent, idleft, idleft.toString, leftLabel)
    tree.addBranch(currParent, idright,idright.toString,rightLabel)

    shift { k: (Unit => Unit) =>
      if (!isFailed) {
        sc.addChoice(new MyContinuation("right", {
          if (!isFailed) {
            beforeBranch()
            currParent = idright
            right
            afterBranch()
          }
          if (!isFailed()) k()
        }))
      }
      if (!isFailed()) {
        beforeBranch()
        currParent = idleft
        left
        afterBranch()
      }
      if (!isFailed()) k()
    }
  }

  def branchAll[A](indexes: Seq[A])(f: A => Unit) = {

    shift { k: (Unit => Unit) =>
      val first = indexes.head
      for (i <- indexes.reverse; if (i != first)) {
        sc.addChoice(new MyContinuation("i", {
          beforeBranch()
          f(i)
          afterBranch()
          if (!isFailed()) k()
        }))

      }
      beforeBranch()
      f(first)
      afterBranch()
      if (!isFailed()) k()
    }
  }

  def branchOne(left: => Unit): Unit @suspendable = {
    shift { k: (Unit => Unit) =>
      left
      if (!isFailed()) k()
    }
  }

  case class Exploration(val exploration: () => Unit @suspendable)
  private var exploBlock: Option[Exploration] = None

  def exploration(block: => Unit @suspendable): ReversibleSearchNode = {
    exploBlock = Option(Exploration(() => block))
    this
  }

  var explorationCompleted = false

  /**
   * Start the exploration block
   * @param: nbSolMax is the maximum number of solution to discover before the exploration stops (default = Int.MaxValue)
   * @param: failureLimit is the maximum number of backtracks before the exploration stops (default = Int.MaxValue)
   */
  def run(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue) = runSubjectTo(nbSolMax, failureLimit)()

  protected def update() = {}
  
  /**
   * Start the exploration block
   * @param: nbSolMax is the maximum number of solution to discover before the exploration stops (default = Int.MaxValue)
   * @param: failureLimit is the maximum number of backtracks before the exploration stops (default = Int.MaxValue)
   * @param: reversibleBlock is bloc of code such that every constraints posted in it are removed after the run
   */
  def runSubjectTo(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue)(reversibleBlock: => Unit = {}): Unit = {
    val t1 = System.currentTimeMillis()
    explorationCompleted = false
    failLimit = failureLimit
    var n = 0
    reset {
      shift { k1: (Unit => Unit) =>
        val b = () => {
          sc.start()
          update()
          //propagate()
          if (!isFailed()) {
            exploBlock.get.exploration()
          } else {
            shift { k: (Unit => Unit) => k() }
          }
          if (!isFailed()) {
            n += 1 // one more sol found
            solFound()
            if (n == nbSolMax) {
              bkts = sc.nFail
              sc.reset() // stop the search
              k1() // exit the exploration block
            }
          }
        }
        popAll()
        pushState()
        reversibleBlock
        sc.reset()
        if (!isFailed()) {
          sc.reset()
          reset {
            b()
          }
          if (!sc.exit) sc.explore() // let's go, unless the user decided to stop
        }
        k1() // exit the exploration block       
      }
    }
    bkts = sc.nFail
    time = System.currentTimeMillis() - t1
    if (!sc.isLimitReached) {
      explorationCompleted = true
    }
    if (explorationCompleted) {
      if (!silent) print("R")
    } else {
      if (!silent) print("!")
    }
  }

}