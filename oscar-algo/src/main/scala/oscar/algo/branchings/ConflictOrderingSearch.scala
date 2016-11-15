package oscar.algo.branchings

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search._
import oscar.algo.vars.IntVarLike

/*
 *  Conflict Ordering Search, basically orders the variables by latest conflict
 *  and assigns the latest conflicting one. 
 */

object ConflictOrderingSearch {
  def apply(variables: Array[IntVarLike], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false) =
    new ConflictOrderingSearch(variables, varHeuristic, valHeuristic, doReset)
}

class ConflictOrderingSearch(variables: Array[IntVarLike], varHeuristic: (Int) => Int, valHeuristic: (Int) => Int, doReset: Boolean = false) extends Branching {
  
  require(variables.length > 0)
  
  private[this] val context = variables(0).context
  
  var lastVariables = List[Int]()
  
  var lastVariable: Option[Int] = None
  var lastDepth = 0
  val depth = new ReversibleInt(context, 0)
  
  override def reset() = {
    lastVariable = None
    lastDepth = 0
    if (doReset) lastVariables = List[Int]()
  }

  override def alternatives(): Seq[Alternative] = {
    val d = depth.incr
    
    // Step 1: if last conflicting variable is new, add in head position.
    if (d <= lastDepth) lastVariable foreach { x =>
      // move x to head if it is in lastVariables
      lastVariables = lastVariables filter (_ != x)      
      lastVariables = x +: lastVariables
      
      lastVariable = None
    }
    
    lastDepth = d
    
    // Step 2: if some variable in conflict set is not bound, branch on it
    lastVariables.foreach { i =>
      val x = variables(i)
      if (!x.isBound) {
        lastVariable = Some(i)
        val value = valHeuristic(i)
        val alternatives = branch { context.assign(x, value) } { context.remove(x, value) }
        return alternatives
      }
    }
    
    // Step 3: if all conflict set variables are bound, ask heuristic
    var bestVar = -1
    var bestScore = Int.MaxValue
    var p = variables.length
    while (p > 0) {
      p -= 1
      if (!variables(p).isBound) {
        val score = varHeuristic(p) 
        if (score <= bestScore) {
          bestVar = p
          bestScore = score
        }
      }
    }
    
    
    if (bestVar == -1) {  // all variables where bound, solution!
      lastVariable = None
      noAlternative
    }
    else {
      lastVariable = Some(bestVar)
      val x = variables(bestVar)
      val value = valHeuristic(bestVar)
      val alternatives = branch { context.assign(x, value) } { context.remove(x, value) }
      alternatives        
    }
  }
}
