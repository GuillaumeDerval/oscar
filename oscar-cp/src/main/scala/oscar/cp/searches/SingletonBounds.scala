package oscar.cp.searches

import oscar.algo.search._
import oscar.cp.core.CPOutcome._
import oscar.cp._

/*
 * Forces singleton-consistency on bounds of given variables.
 * Use this before a real branching, to extend fixed point.
 */

class SingletonBounds(val vars: Array[CPIntVar])(implicit val store: CPStore)
extends Branching with BranchingUtils {
  val nVars = vars.length
  
  def alternatives(): Seq[Alternative] = {
    var hasRemoved = false
    
    // for all variables, shave.
    for (x <- vars) if (!x.isBound) {
      val xMin = x.min
      val xMax = x.max
      var minBound = xMin
      var maxBound = xMax
      
      var removeMin = true
      while (removeMin && minBound < xMax) {
        val xTest = minBound
        store.pushState()
        if (store.post(x.eq(xTest)) == Failure) {
          minBound += 1
          hasRemoved = true
        }
        else removeMin = false
        store.pop()        
      }
      
      if (store.post(x >= minBound) == Failure) return branchOne({})
      
      
      var removeMax = true
      while (removeMax && maxBound > xMin) {
        val xTest = maxBound
        store.pushState()
        if (store.post(x.eq(xTest)) == Failure) {
          maxBound -= 1
          hasRemoved = true
        }
        else removeMax = false
        store.pop()        
      }
      
      if (store.post(x <= maxBound) == Failure) return branchOne({})
    }
    
    if (hasRemoved) { 
      alternatives() 
    }
    else noAlternative
  }
  
}

object SingletonBounds {
  def apply(vars: Array[CPIntVar])(implicit store: CPStore) = new SingletonBounds(vars)
}