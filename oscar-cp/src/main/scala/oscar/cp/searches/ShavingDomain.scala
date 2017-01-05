package oscar.cp.searches

import oscar.algo.search._
import oscar.cp._
import oscar.cp.core.CPStore
import oscar.algo.reversible.SparseSet
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint

/*
 *  A shaving implemented as a branching.
 *    
 *  Not tested extensively, not optimized at all.
 */


class ShavingDomain(val vars: Array[CPIntVar])
             (implicit val store: CPStore)
extends Branching with BranchingUtils {
  val nVars = vars.length
  
  def alternatives(): Seq[Alternative] = {
    var hasRemoved = false
    
    // for all variables, shave.
    for (x <- vars) if (!x.isBound) {
      // Step 1: find all variable-values to shave for variable i
      val unboundVars = vars.filter(!_.isBound)
      var toRemove = unboundVars.map { y => (y, y.toList) }  // initialize with universe
      
      val xDomain = x.toArray
      var p = 0
      while (p < xDomain.length && !toRemove.isEmpty) {  // intersect with x == v, stop if no variable has a candidate value to remove
        val v = xDomain(p)
        store.pushState()
        
        if (store.post(x.eq(v)) != Failure) {        // if this fails, intersect with universe, i.e. do nothing
          toRemove = toRemove.map { case (xr, xrvalues) =>
            (xr, xrvalues.filter { !xr.hasValue(_) })       // if a value is still there, it should not be removed
          }
          
          toRemove = toRemove.filter(!_._2.isEmpty)         // filter out variables that have no values to remove
        }
        
        store.pop()
        p += 1
      }
      
      if (!toRemove.isEmpty) {
        //println(toRemove.mkString("\n"))
        hasRemoved = true
      }
      
      // Step 2: remove values for i
      while (!store.isFailed() && !toRemove.isEmpty) {
        val (xr, xrToRemove) = toRemove.head
        
        val removalConstraints = xrToRemove.map(xr.diff(_): Constraint).toArray
        store.post(removalConstraints)
        
        toRemove = toRemove.tail
      }
      if (store.isFailed) return branchOne()
    }
    
    
    // if some variable-value was filtered, reiterate. Otherwise, give back control to next branching.
    if (hasRemoved) alternatives() else noAlternative
  }
  
}

object ShavingDomain {
  def apply(vars: Array[CPIntVar])(implicit store: CPStore) = new ShavingDomain(vars)
}