package oscar.cp.lcg.searches

import oscar.cp.lcg.variables.LCGIntervalVar

class StaticMinHeuristic(variables: Array[LCGIntervalVar]) extends Heuristic {
  
  require(variables.length > 0, "no variable")
  
  private[this] val x = variables.toIndexedSeq
  private[this] val lcgStore = variables(0).lcgStore
  
  final override def decision: Function0[Unit] = {
    x.foreach(_.updateAndNotify())
    var i = 0
    while (i < variables.length && variables(i).isAssigned) i += 1
    if (i == variables.length) null
    else {
      val x = variables(i)
      val value = x.min
      val literal = x.lowerEqual(value)
      () => {
        println("decision  : assign " + x.name + " to " + value)
        lcgStore.enqueue(literal, null)
      }
    }
  }
}