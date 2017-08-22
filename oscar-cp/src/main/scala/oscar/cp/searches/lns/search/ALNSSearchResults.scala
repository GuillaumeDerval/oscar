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

  override def toString: String = {
    val s = new StringBuilder
    s.append(
      if(unsat) "Unsatisfiable\n"
      else if(solutions.isEmpty) "No solution found\n"
      else "Last " + solutions.last + "\n\toptimum: " + (if(optimumFound) "true\n" else "unknown\n")
    )
    if(relaxStats.nonEmpty){
      s.append("Relaxation operators stats:\n")
      s.append(relaxStats.mkString("\n"))
      s.append("\nSearch operators stats:\n")
    }
    s.append(searchStats.mkString("\n"))
    s.mkString
  }
}
