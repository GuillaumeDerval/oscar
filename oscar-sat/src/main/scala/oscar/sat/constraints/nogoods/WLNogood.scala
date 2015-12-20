package oscar.sat.constraints.nogoods

import oscar.sat.core.CDCLStore
import oscar.sat.core.True
import oscar.sat.core.False
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt

final class WLNogood(store: CDCLStore, literals: Array[Int]) extends Nogood {

  override var activity: Double = 0

  override def locked: Boolean = store.assignReason(literals(0) / 2) == this

  final override def nLiterals = literals.length
  
  override def explain(litId: Int, outReason: ArrayStackInt): Unit = {
    assert(litId == literals(0), s"$litId has not been propagated by this clause.")
    var i = 1
    while (i < literals.length) {
      outReason.append(literals(i) ^ 1)
      i += 1
    }
    store.claBumpActivity(this)
  }

  override def setup(): Boolean = {
    
    // Fast access
    val levels = store.levels

    var maxLit = 0
    var max = -1
    var j = literals.length
    
    // Pick a second literal to watch
    while (j > 1) {
      j -= 1
      val level = levels(literals(j) / 2)
      if (max < level) {
        max = level
        maxLit = j
      }
    }
    
    val tmp = literals(1)
    literals(1) = literals(maxLit)
    literals(maxLit) = tmp

    // Bumping
    store.claBumpActivity(this)
    var i = 0
    while (i < literals.length) {
      store.varBumpActivity(literals(i))
      i += 1
    }

    store.watch(this, literals(0) ^ 1)
    store.watch(this, literals(1) ^ 1)

    // Propagate
    store.enqueue(literals(0), this)
  }

  override def propagate(literal: Int): Boolean = {
    // Make sure the false literal is literals(1)
    if (literals(0) == (literal ^ 1)) {
      literals(0) = literals(1)
      literals(1) = literal ^ 1
    }

    // If 0th watch is true, then clause is already satisfied
    if (store.litValue(literals(0)) == True) {
      store.watch(this, literal)
      return true
    }

    // Look for a new literal to watch
    var i = 2
    while (i < literals.length) {
      if (store.litValue(literals(i)) != False) {
        literals(1) = literals(i)
        literals(i) = literal ^ 1
        store.watch(this, literals(1) ^ 1)
        return true
      }
      i += 1
    }

    // Clause is unit under assignment
    store.watch(this, literal)
    store.enqueue(literals(0), this)
  }

  override def toString: String = literals.mkString(" ")
}