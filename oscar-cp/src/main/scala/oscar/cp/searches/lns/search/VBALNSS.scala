package oscar.cp.searches.lns.search

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable

/**
  * Virtual Best ALNS Solver
  */
class VBALNSS(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config){

  var iterStart = 0L
  val iterSols = new mutable.ListBuffer[CPIntSol]()
  var cumulatedTime = 0L

  override val endTime = Long.MaxValue

  override def onSolution(): Unit = {
    val time = cumulatedTime + (System.nanoTime() - iterStart)

    if(maximizeObjective.isDefined) {
      currentSol = Some(new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time))
      optimumFound = solver.objective.isOptimum() || (config.objective.isDefined && ((maximizeObjective.get && solver.objective.objs.head.best >= config.objective.get) || (!maximizeObjective.get && solver.objective.objs.head.best <= config.objective.get)))
      if (bestSol.isEmpty || (maximizeObjective.get && currentSol.get.objective > bestSol.get.objective) || (!maximizeObjective.get && currentSol.get.objective < bestSol.get.objective)) {
        previousBest = bestSol
        bestSol = currentSol
        iterSols += currentSol.get
      }
    }
    else{
      currentSol = Some(new CPIntSol(vars.map(_.value), 0, time))
      optimumFound = true //In case of CSP, no point of searching another solution
      bestSol = currentSol
      iterSols += currentSol.get
    }
  }

  override def alnsLoop(): Unit = {
    solsFound ++= iterSols
    iterTimeout = config.timeout

    if (!solver.silent){
      println("\nStarting VBS...")
      println("n operators: " + nOpCombinations)
    }

    while(cumulatedTime < config.timeout && !optimumFound){
      val startSol = bestSol.get
      var tUsed = Long.MaxValue
      var bestPerf = 0.0
      val solsKept = new mutable.ListBuffer[CPIntSol]()

      for {
        relax <- getReifiedOperators(relaxOps)
        search <- getReifiedOperators(searchOps)
      } {
        iterStart = System.currentTimeMillis()

        lnsIter(relax, search)

        val execStats = relax.lastExecStats
        val time = if(execStats.isDefined) execStats.get.time else 0L
        val improvement = if(execStats.isDefined) execStats.get.improvement else 0
        val perf = improvement / (time + 1L)
        if(perf < bestPerf || (perf == bestPerf && time < tUsed)){
          bestPerf = perf
          tUsed = time
          solsKept.clear()
          solsKept ++= iterSols
        }

        solver.objective.objs.head.relax()
        solver.objective.objs.head.best = startSol.objective
        currentSol = Some(startSol)
      }

      cumulatedTime += tUsed
      solsFound ++= solsKept
      bestSol = if(solsKept.nonEmpty) Some(solsKept.last) else Some(startSol)
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = bestSol.get.objective
      currentSol = bestSol
      println("Remaining time: " + (config.timeout - cumulatedTime) / 1000000000 + "s")
    }

    stopSearch = true
  }
}
