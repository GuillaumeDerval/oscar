package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSOperator

class ALNSSearchResults(
                         val solutions:Array[CPIntSol],
                         val relaxOperators: Array[ALNSOperator],
                         val searchOperators: Array[ALNSOperator],
                         val optimumFound: Boolean,
                         val unsat: Boolean
                       ) {

  def this(solutions:Array[CPIntSol], operators: Array[ALNSOperator], optimumFound: Boolean, unsat: Boolean){
    this(solutions, Array(), operators, optimumFound, unsat)
  }

  def this(solutions:Array[CPIntSol], optimumFound: Boolean, unsat: Boolean){
    this(solutions, Array(), Array(), optimumFound, unsat)
  }

  override def toString: String = {
    val s = new StringBuilder
    s.append(
      if(unsat) "Unsatisfiable\n"
      else if(solutions.isEmpty) "No solution found\n"
      else "Last " + solutions.last + "\n\toptimum: " + (if(optimumFound) "true\n" else "unknown\n")
    )
    if(relaxOperators.nonEmpty){
      s.append("Relaxation operators stats:\n")
      s.append(relaxOperators.mkString("\n"))
      s.append("\nSearch operators stats:\n")
    }
    s.append(searchOperators.mkString("\n"))
    s.mkString
  }
}
