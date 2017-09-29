package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.operators.ALNSOperator
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
                  val strategy: String = "default", //The ALNS search strategy to use
                  val opDeactivation: Boolean = false //Whether Failing operators can be deactivated or not
                ) {
}
