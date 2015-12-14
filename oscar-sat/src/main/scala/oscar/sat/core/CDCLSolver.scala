package oscar.sat.core

import oscar.sat.heuristics.Heuristic
import oscar.sat.constraints.Constraint

class CDCLSolver extends CDCLStore {
  
  var solution: Array[Boolean] = null
  var totalConfict = 0

  final def solve(h: Heuristic): Boolean = {
    heuristic = h
    var nofConflicts = 100
    var nofLearnts = constraints.size / 3
    var status: LiftedBoolean = Unassigned

    while (status == Unassigned) {
      status = search(nofConflicts, 0.98, 0.999)
      nofConflicts += nofConflicts / 2
      nofLearnts += nofLearnts / 10
    }

    untrailAll()
    status == True
  }
  
  final def search(nofConflict: Int, initVarDecay: Double, initClaDecay: Double): LiftedBoolean = {
    
    var conflictC = 0
    variableDecay = initVarDecay
    activityDecay = initClaDecay
    
    heuristic.init()
    
    var complete = false
    
    while (!complete) {
      val conflict = propagate()
      
      // Conflict to learn
      if (conflict != null) {
        conflictC += 1
        totalConfict += 1
        if (decisionLevel == 0) return False
        else handleConflict(conflict) 
      }
      // No conflict
      else {
      
        // No root simplification
        // No filtering of learnt clause
        
        if (nAssigns() == nVars) {
          // Model found
          solution = Array.tabulate(nVars)(i => isTrue(i))
          untrailAll()
          return True
        }
        else if (conflictC >= nofConflict) {
          // Reached bound on number of conflicts
          untrailAll()
          return Unassigned
        }
        else {
          // Search heuristic
          val literal = heuristic.nextLiteral()
          assume(literal)
        } 
      }
    }
    
    Unassigned
  }

  
}