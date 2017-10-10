package oscar.cp.searches.lns.search

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

object ALNSSearch{
  def apply(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig): ALNSSearch = config.strategy match{
    case "vbs" => new VBALNSS(solver, vars, config)
    case "diveAndExplore" => new ALNSDiveAndExplore(solver, vars, config)
    case "efficiencyBasedLaborie" => new EfficiencyBasedLaborie(solver, vars, config)
    case _ => new ALNSSearchImpl(solver, vars, config)
  }
}

/**
  * Adaptive lage neighbourhood search interface.
  */
abstract class ALNSSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig){

  /**
    * Performs the alns search from a specific solution.
    * @return an ALNSSearchResult object containing the solutions found along with some statistics on the search.
    */
  def searchFrom(sol: CPIntSol): ALNSSearchResults

  /**
    * Performs the alns search.
    * @return an ALNSSearchResult object containing the solutions found along with some statistics on the search.
    */
  def search(): ALNSSearchResults
}
