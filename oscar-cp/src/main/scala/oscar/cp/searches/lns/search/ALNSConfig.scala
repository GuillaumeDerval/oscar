package oscar.cp.searches.lns.search

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.operators.{ALNSBuilder, ALNSElement, ALNSOperator}
import oscar.cp.searches.lns.selection.AdaptiveStore

/**
  * Search configuration
  */
class ALNSConfig(
                  val timeout: Long = 0, //Search timeout (ns)
                  val objective: Option[Int] = None, //Objective threshold to reach
                  val memLimit: Int = 1000, //Memory limit (mb)
                  val coupled: Boolean = false, //Coupled operators
                  val learning: Boolean = false, //Learning phase
                  val relaxStore: Option[AdaptiveStore[ALNSOperator]], //The relax operators to use
                  val searchStore: AdaptiveStore[ALNSOperator], //The search operators to use
                  val opDeactivation: Boolean = false, //Wether Failing operators can be deactivated or not
                  val metric: (ALNSElement, Int, SearchStatistics) => Double //The operators performance metric
                ) {
}
