package oscar.sat.heuristics

import oscar.algo.array.ArrayHeapDouble
import oscar.sat.core.CDCLStore

abstract class Heuristic {

  def undo(varId: Int): Unit

  def nextLiteral(): Int

  def init(): Unit = Unit

  def updateActivity(varId: Int): Unit
}