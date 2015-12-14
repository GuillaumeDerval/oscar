package oscar.sat.utils

import scala.io.Source
import oscar.sat.core.CDCLStore
import oscar.sat.heuristics.ActivityHeuristic
import oscar.sat.core.CDCLSolver
import scala.collection.mutable.ArrayBuffer

class DimacsFile(final val nVariables: Int, final val clauses: Array[Array[Int]]) {
  
  final val nClauses: Int = clauses.length
  
  final def model: CDCLSolver = {
    val solver = new CDCLSolver()
    // Creates literals
    val literals = Array.tabulate(nVariables)(i => solver.newVar())
    // Creates clauses
    Array.tabulate(nClauses)(c => {
      val clause = clauses(c).map(i => {
        if (i < 0) (-i-1) * 2 + 1
        else (i - 1) * 2
      })
      solver.addClause(clause)
    })
    // Return the model
    solver
  }
  
  final def verify(assignment: Array[Boolean]): Boolean = {
    var i = 0
    while (i < clauses.length) {
      val clause = clauses(i)
      var j = 0
      var satisfied = false
      while (j < clause.length) {
        val lit = clause(j)
        val signed = lit < 0
        val id = if (signed) -lit - 1 else lit - 1
        val value = if (signed) !assignment(id) else assignment(id)
        satisfied |= value
        j += 1
      }
      if (satisfied == false) {
        println("clause " + i + " is not satisfied")
        return false
      }
      i += 1
    }
    true
  }
}

object DimacsFile {
  
  final def parse(dimacsFile: String): DimacsFile = {
    val lines = Source.fromFile(dimacsFile).getLines
    if (lines.isEmpty) null
    else {
      
      // Data
      val clauses = ArrayBuffer[Array[Int]]()
      var maxLitId = 0
      
      while (lines.hasNext) {
        val line = lines.next.replaceAll("\\s+", " ")
        val firstChar = line.charAt(0)
        if (firstChar != 'p' && firstChar != 'c') {
          val data = line.trim.split("\\s+")
          val clause = new Array[Int](data.length - 1)
          var i = clause.length
          while (i > 0) {
            i -= 1
            val litId = java.lang.Integer.parseInt(data(i))
            maxLitId = Math.max(maxLitId, litId)
            clause(i) = litId
          }
          clauses.append(clause)
        }
      }
      
      new DimacsFile(maxLitId, clauses.toArray)
    }
  }
}