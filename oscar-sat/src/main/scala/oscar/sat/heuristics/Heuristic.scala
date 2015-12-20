package oscar.sat.heuristics

trait Heuristic {

  def undo(varId: Int): Unit

  def nextLiteral(): Int

  def init(): Unit = Unit

  def updateActivity(varId: Int): Unit
}