package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.CPIntSol

class ALNSSearchResults(
                         val solutions:Array[CPIntSol],
                         val relaxStats: Map[String, ALNSStatistics],
                         val searchStats: Map[String, ALNSStatistics]
                       ) {

  def this(solutions:Array[CPIntSol], operatorStats: Map[String, ALNSStatistics]){
    this(solutions, Map(), operatorStats)
  }
}
