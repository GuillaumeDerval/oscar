package oscar.sat.examples

import oscar.sat.utils.DimacsFile
import oscar.sat.heuristics.ActivityHeuristic
import oscar.sat.constraints.clauses.Clause

object TestGraph extends App {

  // Data
  val instance = DimacsFile.parse("data/dimacs/queens10.cnf")
  val solver = instance.model
  val heuristic = new ActivityHeuristic(instance.nVariables, solver)

  // Out 1 
  println("#variables  : " + instance.nVariables)
  println("#clauses    : " + instance.nClauses)
  
  // Solve
  val t0 = System.nanoTime()

  var nSols = 0  
  while (solver.solve(heuristic)) {
    
    nSols += 1

    /*for (j <- 0 to 90 by 10) {
      for (i <- 0 to 9) {
        if (solver.solution(j + i)) print("X ") else print(". ")
      }
      println()
    }

    println()
    println("--------------------")
    println()*/

    solver.add(Clause(solver, Array.tabulate(100)(i => {
      if (solver.solution(i)) i * 2 + 1
      else i * 2
    })))
  }
  
  val t1 = System.nanoTime() - t0
  
    // Out
  println("#solutions  : " + nSols)
  println("#conflicts  : " + solver.totalConfict)
  println("time (ms)   : " + (t1 / 1000000.0)) 
}