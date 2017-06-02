package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.CPIntSol

class ALNSSearchResults(
                         val solutions:Array[CPIntSol],
                         val relaxStats: Map[String, ALNSStatistics],
                         val searchStats: Map[String, ALNSStatistics],
                         val optimumFound: Boolean
                       ) {

  def this(solutions:Array[CPIntSol], operatorStats: Map[String, ALNSStatistics], optimumFound: Boolean){
    this(solutions, Map(), operatorStats, optimumFound)
  }

  def this(solutions:Array[CPIntSol], optimumFound: Boolean){
    this(solutions, Map(), Map(), optimumFound)
  }
}
