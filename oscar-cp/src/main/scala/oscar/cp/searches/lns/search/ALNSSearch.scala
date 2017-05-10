package oscar.cp.searches.lns.search

import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp.CPIntVar
import oscar.cp.core.CPStore
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSBuilder, ALNSElement, SearchFunctions}

import scala.collection.mutable

object ALNSSearch{
  def apply(vars: Array[CPIntVar], objective: CPIntVar, config: ALNSConfig): ALNSSearch =
    if(config.coupled) new ALNSCoupledSearch(vars, objective, config)
    else new ALNSLooseSearch(vars, objective, config)
}

/**
  * Adaptive lage neighbourhood search framework.
  * Instantiates a CPModel and performs an ALNS search over it.
  */
abstract class ALNSSearch(vars: Array[CPIntVar], objective: CPIntVar, config: ALNSConfig) {

  val startTime: Long = System.nanoTime()
  val endTime: Long = startTime + config.timeout
  var endSearch: Long = endTime

  val cp: CPStore = objective.store

  val maximizeObjective: Boolean = false //TODO: detect this
  if(!cp.silent) println("Objective type: " + (if(maximizeObjective) "max" else "min"))

  //What to do on solution:
  val solsFound = new mutable.ListBuffer[CPIntSol]()
  var currentSol: CPIntSol = new CPIntSol(new Array[Int](0), if(maximizeObjective) Int.MinValue else Int.MaxValue, 0)  //Current solution
  var bestSol: CPIntSol = currentSol //Best solution so far

  cp.onSolution{
    val time = System.nanoTime() - startTime
    currentSol = new CPIntSol(vars.map(_.value), objective.value, time)
    if((maximizeObjective && currentSol.objective > bestSol.objective) || (!maximizeObjective && currentSol.objective < bestSol.objective)){
      bestSol = currentSol
      solsFound += currentSol
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

  val builder = new ALNSBuilder(cp, vars, maximizeObjective, config)

  val metric: (ALNSElement, Int, SearchStatistics) => Double = builder.instantiateMetric()

  /**
    * Performs the alns search.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def search(): ALNSSearchResults = {

    //1st solution:
    if(!cp.silent) println("Starting first solution search...")

    val stats = cp.startSubjectTo(stopCondition, Int.MaxValue, null){
      cp.search(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = false))
    }

    if(!cp.silent) println("Time elapsed: " + (System.nanoTime() - startTime)/1000000000.0 + "s")

    //learning phase:
    if(config.learning){
      if(!cp.silent) println("\nStarting learning phase...")
      alnsLearning()
      if(!cp.silent){
        println("Learning phase done.")
        println("Time elapsed: " + (System.nanoTime() - startTime)/1000000000.0 + "s")
      }
    }

    //LNS loop
    if(!cp.silent) println("\nStarting adaptive LNS...")
    alnsLoop()

    ALNSElement.resetWorstTTI()
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
    * Creates the search results object.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def getSearchResults: ALNSSearchResults
}
