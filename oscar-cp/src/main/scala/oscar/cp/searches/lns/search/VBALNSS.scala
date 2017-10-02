package oscar.cp.searches.lns.search

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable

/**
  * Virtual Best ALNS Solver
  */
class VBALNSS(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config){

  override val endTime = Long.MaxValue

  override def alnsLoop(): Unit = {
    if (!solver.silent){
      println("\nStarting VBS...")
      println("n operators: " + nOpCombinations)
    }
    var cumulatedTime = 0L
    val keptSols = new mutable.ListBuffer[CPIntSol]()
    keptSols ++= solsFound

    while(cumulatedTime < config.timeout && !optimumFound){
      val startSol = bestSol.get
      var tUsed = Long.MaxValue
      var bestPerf = 0.0

      for {
        relax <- getReifiedOperators(relaxOps)
        search <- getReifiedOperators(searchOps)
      } {
        lnsIter(relax, search)

        val time = relax.lastExecStats.time
        val improvement = relax.lastExecStats.improvement
        val perf = improvement / (time + 1L)
        if(perf < bestPerf){
          bestPerf = perf
          tUsed = time
        }
        else if(perf == bestPerf) tUsed = Math.min(tUsed, time)

        solver.objective.objs.head.relax()
        solver.objective.objs.head.best = startSol.objective
        currentSol = Some(startSol)
      }

      cumulatedTime += tUsed
      println("Remaining time: " + (config.timeout - cumulatedTime) / 1000000000 + "s")
      keptSols += new CPIntSol(bestSol.get.values, bestSol.get.objective, cumulatedTime)
    }

    solsFound.clear()
    solsFound ++= keptSols
    stopSearch = true
  }
}
