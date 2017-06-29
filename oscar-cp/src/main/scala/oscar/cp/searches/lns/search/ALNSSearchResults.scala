package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.CPIntSol

class ALNSSearchResults(
                         val solutions:Array[CPIntSol],
                         val relaxStats: Map[String, ALNSStatistics],
                         val searchStats: Map[String, ALNSStatistics],
                         val optimumFound: Boolean,
                         val unsat: Boolean
                       ) {

  def this(solutions:Array[CPIntSol], operatorStats: Map[String, ALNSStatistics], optimumFound: Boolean, unsat: Boolean){
    this(solutions, Map(), operatorStats, optimumFound, unsat)
  }

  def this(solutions:Array[CPIntSol], optimumFound: Boolean, unsat: Boolean){
    this(solutions, Map(), Map(), optimumFound, unsat)
  }
}
