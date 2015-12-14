package oscar.sat.constraints.nogoods

import oscar.sat.core.CDCLStore
import oscar.algo.array.ArrayStackInt

final class BinaryNogood(store: CDCLStore, _lit1: Int, _lit2: Int) extends Nogood {

  override var activity: Double = 0

  override def locked: Boolean = store.assignReason(lit1 / 2) == this

  private[this] var lit1: Int = _lit1 // the unit literal
  private[this] var lit2: Int = _lit2

  override def remove(): Unit = Unit

  override def simplify(): Boolean = true

  override def explain(litId: Int, outReason: ArrayStackInt): Unit = {
    assert(litId == lit1, s"$litId has not been propagated by this clause.")
    outReason.append(lit2 ^ 1)
  }

  override def explainAll(outReason: ArrayStackInt): Unit = {
    outReason.append(lit1 ^ 1)
    outReason.append(lit2 ^ 1)
  }

  override def setup(): Boolean = {

    // Bumping
    store.claBumpActivity(this)
    store.varBumpActivity(lit1)
    store.varBumpActivity(lit2)

    // Watch   
    store.watch(this, lit1 ^ 1)
    store.watch(this, lit2 ^ 1)

    // Propagate
    store.enqueue(lit1, this)
  }

  override def propagate(literal: Int): Boolean = {

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

  override def toString: String = lit1 + " " + lit2
}