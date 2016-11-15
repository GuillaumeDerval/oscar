package oscar.algo.branchings

import oscar.algo.search.Outcome._
import oscar.algo.search._
import oscar.algo.vars.IntVarLike

/*
 * Forces singleton-consistency on bounds of given variables.
 * Use this before a real branching, to extend fixed point.
 */

class SingletonBounds(val vars: Array[IntVarLike]) extends Branching with BranchingUtils {
  val context = vars(0).context
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
        context.pushState()
        if (context.assign(x, xTest) == Failure) {
          minBound += 1
          hasRemoved = true
        }
        else removeMin = false
        context.pop()
      }
      
      if (context.largerEq(x, minBound) == Failure) return branchOne({})
      
      
      var removeMax = true
      while (removeMax && maxBound > xMin) {
        val xTest = maxBound
        context.pushState()
        if (context.assign(x, xTest) == Failure) {
          maxBound -= 1
          hasRemoved = true
        }
        else removeMax = false
        context.pop()
      }
      
      if (context.smallerEq(x, maxBound) == Failure) return branchOne({})
    }
    
    if (hasRemoved) { 
      alternatives() 
    }
    else noAlternative
  }
  
}

object SingletonBounds {
  def apply(vars: Array[IntVarLike])(implicit store: IntVarLike) = new SingletonBounds(vars)
}