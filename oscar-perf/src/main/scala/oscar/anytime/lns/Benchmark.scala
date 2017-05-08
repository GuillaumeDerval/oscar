package oscar.anytime.lns

import oscar.cp._
import oscar.cp.core.variables.CPIntVar

trait Benchmark {

  def decisionVariables: Array[CPIntVar]
  def objective: CPIntVar
  def bestKnownObjective: Int

  def main(args: Array[String]): Unit = {
    val cp = objective.store

    cp.onSolution {
      println("solution")
    }

       cp.search(binaryStatic(decisionVariables))

    cp.start()

  }

}

