package oscar.cp.searches.lns.search

import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSBuilder, ALNSElement, SearchFunctions}

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
  val endTime: Long = startTime + config.timeout
  var endSearch: Long = endTime

  var learnRatio = 0.3 //The ratio of remaining time that a learning phase will have
  var learning = false
  var iterTimeout: Long = config.timeout
  var searchFail = 0
  var stagnation = 0
  val stagnationThreshold = 100

  val maximizeObjective: Boolean = solver.objective.objs.head.isMax //TODO: deal with CSP
  if(!solver.silent) println("Objective type: " + (if(maximizeObjective) "max" else "min"))

  //What to do on solution:
  val solsFound = new mutable.ListBuffer[CPIntSol]()
  var currentSol: CPIntSol = new CPIntSol(new Array[Int](0), if(maximizeObjective) Int.MinValue else Int.MaxValue, 0, "")  //Current solution
  var bestSol: CPIntSol = currentSol //Best solution so far

  solver.onSolution{
    val time = System.nanoTime() - startTime
    currentSol = new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time, config.solutionGenerator())
    if((maximizeObjective && currentSol.objective > bestSol.objective) || (!maximizeObjective && currentSol.objective < bestSol.objective)){
      bestSol = currentSol
      solsFound += currentSol
      println("o " + currentSol.objective)
    }
  }

  val nSols = 1
  val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
    var stop = false
    stop |= (nSols != 0 && s.nSolutions >= nSols)
    stop |= (System.nanoTime() >= endTime)
    stop |= (System.nanoTime() >= endSearch)
    stop
  }

  val builder = new ALNSBuilder(solver, vars, maximizeObjective, config)

  val metric: (ALNSElement, Int, SearchStatistics) => Double = builder.instantiateMetric()

  /**
    * Performs the alns search.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def search(): ALNSSearchResults = {

    //1st solution:
    if(!solver.silent) println("Starting first solution search...")

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = false))
    }

    if(!solver.silent) println("Time elapsed: " + (System.nanoTime() - startTime)/1000000000.0 + "s")

    while(System.nanoTime() < endTime && !solver.objective.isOptimum() && searchFail < 10) {
      val startObjective = bestSol.objective

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

      if(bestSol.objective == startObjective) searchFail += 1
      else searchFail = 0

      if(!solver.silent) println("ALNS Search done, resetting operators...")
      resetStore()
    }
    ALNSElement.resetWorstTTI()
    if(!solver.silent) println("Search done, retrieving results")
    getSearchResults
  }

  /**
    * Performs a learning phase where each operator is tried at least once from the same solution.
    */
  def alnsLearning(): Unit

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
