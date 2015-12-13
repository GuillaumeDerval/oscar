package oscar.sat.constraints.nogoods

import oscar.sat.core.CDCLStore
import oscar.sat.core.True
import oscar.sat.core.False
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt

final class WLNogood(solver: CDCLStore, literals: Array[Int]) extends Nogood {

  override var activity: Double = 0
  
  override def locked: Boolean = solver.assignReason(literals(0) / 2) == this

  override def remove(): Unit = Unit

  override def simplify(): Boolean = true
  
  override def explain(outReason: ArrayStackInt): Unit = {
    var i = 1
    while (i < literals.length) {
      outReason.append(literals(i) ^ 1)
      i += 1
    }
    solver.claBumpActivity(this)
  }
  
  override def explainAll(outReason: ArrayStackInt): Unit = {
        var i = 0
    while (i < literals.length) {
      outReason.append(literals(i) ^ 1)
      i += 1
    }
    solver.claBumpActivity(this)
  }

  override def propagate(literal: Int): Boolean = {
    // Make sure the false literal is literals(1)
    if (literals(0) == (literal ^ 1)) {
      literals(0) = literals(1)
      literals(1) = literal ^ 1
    }

    // If 0th watch is true, then clause is already satisfied
    if (solver.litValue(literals(0)) == True) {
      solver.watch(this, literal)
      return true
    }

    // Look for a new literal to watch
    var i = 2
    while (i < literals.length) {
      if (solver.litValue(literals(i)) != False) {
        literals(1) = literals(i)
        literals(i) = literal ^ 1
        solver.watch(this, literals(1) ^ 1)
        return true
      }
      i += 1
    }
    
    // Clause is unit under assignment
    solver.watch(this, literal)
    solver.enqueue(literals(0), this)
  }
  
  override def toString: String = literals.mkString(" ")
}