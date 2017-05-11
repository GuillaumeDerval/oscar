package oscar.modeling.solvers.cp.searches.alns.search

import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp.searches.lns.operators.ALNSElement
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearchResults}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.solvers.cp.searches.alns.XCSPIntSol
import oscar.modeling.solvers.cp.searches.alns.operators.{ALNSBuilder, SearchFunctions}
import oscar.modeling.vars.IntVar

import scala.collection.mutable

object ALNSSearch{
  def apply(md: ModelDeclaration, vars: Array[IntVar], config: ALNSConfig, xcspInstantiator: () => String): ALNSSearch =
    if(config.coupled) new ALNSCoupledSearch(md, vars, config, xcspInstantiator)
    else new ALNSLooseSearch(md, vars, config, xcspInstantiator)
}

/**
  * Adaptive lage neighbourhood search framework.
  * Instantiates a CPModel and performs an ALNS search over it.
 *
  * @param md a model declaration
  * @param vars an array of IntVars declared in the model declaration.
  * @param config an ALNSConfig object containing the search configuration information.
  * @param xcspInstantiator tool used to create a XCSP instantiation from a solution.
  */
abstract class ALNSSearch(md: ModelDeclaration, vars: Array[IntVar], config: ALNSConfig, xcspInstantiator: () => String) {

  val startTime: Long = System.nanoTime()
  val endTime: Long = startTime + config.timeout
  var endSearch: Long = endTime

  val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
  val maximizeObjective: Boolean = model.cpObjective.isMax
  println("Objective type: " + (if(maximizeObjective) "max" else "min"))

  //What to do on solution:
  val solsFound = new mutable.ArrayBuffer[XCSPIntSol]()
  var currentSol = new XCSPIntSol(new Array[Int](0), if(maximizeObjective) Int.MinValue else Int.MaxValue, 0, "")  //Current solution
  var bestSol: XCSPIntSol = currentSol //Best solution so far

  def updateSols(): XCSPIntSol = {
    val time = System.nanoTime() - startTime
    currentSol = new XCSPIntSol(
      vars.map(x => x.evaluate()),
      model.cpObjective.best,
      time,
      xcspInstantiator())
    if((maximizeObjective && currentSol.objective > bestSol.objective) || (!maximizeObjective && currentSol.objective < bestSol.objective)){
      bestSol = currentSol
      solsFound += currentSol
    }
    currentSol
  }

  //Stopping condition of each iterative loop:
  val nSols = 1
  val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
    var stop = false
    stop |= (nSols != 0 && s.nSolutions >= nSols)
    stop |= (System.nanoTime() >= endSearch)
    stop
  }

  val builder = new ALNSBuilder(model, vars, maximizeObjective, config)

  val metric: (ALNSElement, Int, SearchStatistics) => Double = builder.instantiateMetric()

  /**
    * Performs the alns search.
    * @return an ALNSSearch result object containing the solutions found along with some statistics on the search.
    */
  def search(): ALNSSearchResults = {

    md.apply(model) {

      model.cpSolver.onSolution {
        updateSols()
      }

      //1st solution:
      println("Starting first solution search...")

      val stats = model.cpSolver.startSubjectTo(stopCondition, Int.MaxValue, null){
        model.cpSolver.search(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = false)(model))
      }

      val initSolTime = System.nanoTime() - startTime
      println("Time elapsed: " + (initSolTime/1000000000.0) + "s")

      //learning phase:
      if(config.learning){
        println("\nStarting learning phase...")
        alnsLearning()
        println("Learning phase done.")
      }

      //LNS loop
      println("\nStarting adaptive LNS...")
      alnsLoop()

      if(solsFound.nonEmpty && ((maximizeObjective && solsFound.last.objective < bestSol.objective) || (!maximizeObjective && solsFound.last.objective > bestSol.objective))) solsFound += bestSol
    }

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
