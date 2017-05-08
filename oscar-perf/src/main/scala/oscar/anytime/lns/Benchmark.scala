package oscar.anytime.lns

import oscar.algo.search.DFSearch
import oscar.anytime.lns.utils.XmlWriter
import oscar.cp._
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable

trait Benchmark {

  def decisionVariables: Array[CPIntVar]
  def objective: CPIntVar
  def bestKnownObjective: Int //Should be an option?

  def main(args: Array[String]): Unit = {
    val cp = objective.store
    val timeout = args(0).toLong * 1000000000L

    val startTime = System.nanoTime()
    val endTime = startTime + timeout
    val solutions = mutable.ListBuffer[(Long, Int)]()

    cp.onSolution {
      val time = System.nanoTime() - startTime //This will have to change in case of parralelisation
      solutions += ((time, objective.value))
      if(!cp.silent) println("Solution found: " + objective.value + " at " + time/1000000000.0 + "s")
    }

    val nSols = 0
    val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
      var stop = false
      stop |= (nSols != 0 && s.nSolutions >= nSols)
      stop |= (System.nanoTime() >= endTime)
      stop
    }

    val stats = cp.startSubjectTo(stopCondition, Int.MaxValue, null) {
      cp.search(binaryStatic(decisionVariables))
    }

    XmlWriter.writeToXml("../ALNS-bench-results/", "testMethod", "testInstance", Some(bestKnownObjective), maxObjective = false, solutions)
  }

}

