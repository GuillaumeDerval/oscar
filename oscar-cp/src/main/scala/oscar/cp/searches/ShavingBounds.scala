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


class ShavingBounds(val vars: Array[CPIntVar])
             (implicit val store: CPStore)
extends Branching with BranchingUtils {
  val nVars = vars.length
  
  val minBound = new Array[Int](nVars)
  val maxBound = new Array[Int](nVars)
  
  def alternatives(): Seq[Alternative] = {
    var hasRemoved = false
    
    val unboundVars = vars.filter(!_.isBound)
    val nUVars = unboundVars.length
    
    // for all variables, compute union of surviving domains after assignment
    for (x <- vars) if (!x.isBound) {
      // Step 1: initialize with empty domains
      var p = 0
      while (p < nUVars) {
        minBound(p) = Int.MaxValue
        maxBound(p) = Int.MinValue
        p += 1
      }
      
      // Step 2: find all variable-values to shave for variable i
      // TODO: use sparse set to remember what variables have union of survivors equal to current domain,
      // remove those from examination, and stop loop if all variables have already full domain
      val xDomain = x.toArray
      p = 0
      while (p < xDomain.length) {
        val v = xDomain(p)
        store.pushState()
        
        if (store.post(x.eq(v)) != Failure) {        // if this fails, union with nothing, achieved by doing nothing
          var q = 0
          while (q < nUVars) {
            minBound(q) = math.min(minBound(q), unboundVars(q).min)
            maxBound(q) = math.max(maxBound(q), unboundVars(q).max)
            q += 1
          }
        }
        
        store.pop()
        p += 1
      }
      
      // Step 3: remove values for i
      var q = 0
      while (q < nUVars && !store.isFailed) {
        val x = unboundVars(q)
        
        if (minBound(q) > x.min || maxBound(q) < x.max) {
          hasRemoved = true
          store.post(Array(x >= minBound(q), x <= maxBound(q)))
        }
        
        q += 1
      }
      if (store.isFailed) return branchOne()
    }
    
    
    // if some variable-value was filtered, reiterate. Otherwise, give back control to next branching.
    if (hasRemoved) alternatives() else noAlternative
  }
  
}

object ShavingBounds {
  def apply(vars: Array[CPIntVar])(implicit store: CPStore) = new ShavingBounds(vars)
}