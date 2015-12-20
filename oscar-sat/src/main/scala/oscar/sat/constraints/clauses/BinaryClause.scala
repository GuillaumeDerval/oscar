package oscar.sat.constraints.clauses

import oscar.sat.core.CDCLStore
import oscar.sat.core.True
import oscar.algo.array.ArrayStackInt

class BinaryClause(store: CDCLStore, _lit1: Int, _lit2: Int) extends Clause {

  private[this] var lit1: Int = _lit1 // the unit literal
  private[this] var lit2: Int = _lit2
  
  final override val nLiterals: Int = 2

  final override def explain(litId: Int, outReason: ArrayStackInt): Unit = {
    assert(litId == lit1, s"$litId has not been propagated by this clause.")
    outReason.append(lit2 ^ 1)
  }
  
  override def setup(): Boolean = {
    store.watch(this, lit1 ^ 1)
    store.watch(this, lit2 ^ 1)
    true
  }

  final override def propagate(literal: Int): Boolean = { 
    
    // Keep watching the literal
    store.watch(this, literal)
    
    // Make sure the false literal is lit2
    if (lit1 == (literal ^ 1)) {
      lit1 = lit2
      lit2 = literal ^ 1
    }
    
    // Propagate
    store.enqueue(lit1, this)
  }
  
  final override def toString: String = lit1 + " " + lit2
}