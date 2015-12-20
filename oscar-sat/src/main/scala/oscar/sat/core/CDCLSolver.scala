package oscar.sat.core

import oscar.sat.heuristics.Heuristic
import oscar.sat.constraints.Constraint

class CDCLSolver extends CDCLStore {
  
  var solution: Array[Boolean] = null
  var totalConfict = 0

  final def solve(h: Heuristic): Boolean = {
    heuristic = h
    var nofConflicts = 100
    var nofLearnts = nConstraints / 3
    var status: LiftedBoolean = Unassigned

    while (status == Unassigned) {
      status = search(nofConflicts, nofLearnts, 0.98, 0.999)
      nofConflicts += nofConflicts / 2
      nofLearnts += nofLearnts / 10
    }

    untrailAll()
    status == True
  }
  
  final def search(nofConflict: Int, nofNogoods: Int, initVarDecay: Double, initClaDecay: Double): LiftedBoolean = {
    
    var conflictC = 0
    variableDecay = initVarDecay
    activityDecay = initClaDecay
    
    heuristic.init()
    
    var complete = false
    
    while (!complete) {
      
      val success = propagate()
      // Conflict to learn
      if (!success) {
        conflictC += 1
        totalConfict += 1
        if (level == 0) return False
        else handleConflict() 
      }
      // No conflict
      else {
      
        // No root simplification
        // No filtering of learnt clause
        
        if (nNogoods - nAssigns() >= nofNogoods) {
          reduceDb()
        }
        
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