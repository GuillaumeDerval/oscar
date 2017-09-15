package oscar.cp.searches.lns.search

import oscar.algo.search.DFSearch
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSElement, SearchFunctions}
import oscar.cp.searches.lns.selection.Metrics

import scala.collection.mutable

object ALNSSearch{
  def apply(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig): ALNSSearch =
    if(config.coupled) new ALNSCoupledSearch(solver, vars, config)
    else new ALNSLooseSearch(solver, vars, config)
}

/**
  * Adaptive lage neighbourhood search framework.
  * Instantiates a CPModel and performs an ALNS search over it.
  */
abstract class ALNSSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) {
  val startTime: Long = System.nanoTime()
  var iter: Long = 1L

  //Configuration:
  val endTime: Long = if(config.timeout > 0) startTime + config.timeout else Long.MaxValue //Maximal allocated time
  var endIter: Long = endTime //Maximal allocated time for the current iteration

  var learnRatio = 0.3 //The ratio of remaining time that a learning phase will have
  var learning = false //Currently in learning phase
  var iterTimeout: Long = if(config.timeout > 0) config.timeout / 300 else Long.MaxValue //The iteration allocated time
  var searchFail = 0
  var stagnation = 0
  val stagnationThreshold = 50

  var nSols = 0
  var nFailures = 0

  val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
  if(!solver.silent) println("Objective type: " + (if(maximizeObjective.isDefined) if(maximizeObjective.get) "max" else "min" else "none"))

  //What to do on solution:
  val solsFound = new mutable.ListBuffer[CPIntSol]()
  var currentSol: Option[CPIntSol] = None  //Current solution
  var bestSol: Option[CPIntSol] = currentSol //Best solution so far
  var previousBest: Option[CPIntSol] = None //Previous best solution
  var optimumFound = false //True if the whole search space has been explored (csp and cop) or the optimum has been found (cop)

  solver.onSolution{
    val time = System.nanoTime() - startTime
    if(maximizeObjective.isDefined) {
      currentSol = Some(new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time))
      optimumFound = solver.objective.isOptimum() || (config.objective.isDefined && (
        (maximizeObjective.get && solver.objective.objs.head.best >= config.objective.get) ||
        (!maximizeObjective.get && solver.objective.objs.head.best <= config.objective.get)
      ))
      if (bestSol.isEmpty || (maximizeObjective.get && currentSol.get.objective > bestSol.get.objective) || (!maximizeObjective.get && currentSol.get.objective < bestSol.get.objective)) {
        previousBest = bestSol
        bestSol = currentSol
        solsFound += currentSol.get
      }
    }
    else{
      currentSol = Some(new CPIntSol(vars.map(_.value), 0, time))
      optimumFound = true //In case of CSP, no point of searching another solution
      bestSol = currentSol
      solsFound += currentSol.get
    }
  }

  val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
    var stop = false
    stop |= nSols != 0 && s.nSolutions >= nSols
    stop |= nFailures != 0 && s.nBacktracks >= nFailures
    stop |= System.nanoTime() >= endTime
    stop |= System.nanoTime() >= endIter
    stop
  }

  def searchFirstSol(): Boolean = {
    if(!solver.silent) println("Starting first solution search...")

    nSols = 2

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(SearchFunctions.conflictOrdering(vars, if(maximizeObjective.isDefined) if(maximizeObjective.get) "Min" else "Max"  else "Max", valLearn = false))
    }

    nSols = 0

    if(!solver.silent) println("Time elapsed: " + (System.nanoTime() - startTime)/1000000000.0 + "s")
    iterTimeout = stats.time * 1000000 * 4
    stats.completed || solver.objective.isOptimum()
  }

  def searchFrom(sol: CPIntSol): ALNSSearchResults = {
    if(maximizeObjective.isDefined) {
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = sol.objective
    }
    currentSol = Some(sol)
    bestSol = Some(sol)

    search()
  }

  /**
    * Performs the alns search.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def search(): ALNSSearchResults = {

    //Searching first solution if needed
    optimumFound = if(currentSol.isEmpty) searchFirstSol() else false

    while(System.nanoTime() < endTime && !optimumFound && searchFail < 10) {
      val startObjective = bestSol.get.objective

      //learning phase:
      if (config.learning) {
        if (!solver.silent) println("\nStarting learning phase...")
        alnsLearning()
        if (!solver.silent) {
          println("Learning phase done.")
          println("Time elapsed: " + (System.nanoTime() - startTime) / 1000000000.0 + "s")
        }
      }

      //LNS loop
      alnsLoop()

      if(bestSol.get.objective == startObjective) searchFail += 1
      else searchFail = 0

      if(!solver.silent) println("ALNS Search done, resetting operators...")
      resetStore()
    }

    if(optimumFound && !solver.silent) println("Optimal solution Found!")
    Metrics.worstTTI = 1.0
    if(!solver.silent) println("Search done, retrieving results")
    val results = getSearchResults
    if(!solver.silent) println(results)
    results
  }

  /**
    * Performs a learning phase where each operator is tried at least once from the same solution.
    */
  def alnsLearning(): Unit

  def onStagnation(): Unit

  /**
    * Performs a parameter configuration phase where each operator is configured using SPOT.
    */
  def spotTuning(): Unit

  /**
    * Performs a sequential ALNS loop
    */
  def alnsLoop(): Unit

  /**
    * resets the store(s)
    */
  def resetStore(): Unit

  /**
    * Creates the search results object.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def getSearchResults: ALNSSearchResults
}
