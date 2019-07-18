package oscar.cp.heuristics

import oscar.cp.CPIntVar
import oscar.cp.core.CPSolver
import oscar.cp._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


/** Bound-Impact Value Selector from "Making the first solution good!"
  *
  * @param cp         The  cp solver
  * @param variables  The variables on which the value heuristic is applied
  * @param maxDomSize Limits the number of values that are considered during the selection process, by default 20
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class BoundImpactValueSelector(cp: CPSolver, variables: Array[CPIntVar], maxDomSize: Int= 20) {

  private[this] val isMinimization:Boolean = cp.objective.objs.head.isMin
  private[this] val threshold = maxDomSize
  private[this] val rand = new Random(42)

  def selectValue(i: Int): Int = {
    // best bound modification so for
    var bestBound = Integer.MAX_VALUE
    // all values that lead to the same best bound modification are candidate values
    var candidates = ArrayBuffer[Int]()
    val values = new Array[Int](variables(i).getSize)
    variables(i).fillArray(values)
    val vSeq = values.toSeq
    val shuffled = rand.shuffle(vSeq)
    val maxCount = if(values.length > threshold) threshold else values.length
    for(j <- 0 until maxCount) {
      val x = shuffled(j)
      val b = bound(i, x)
      // if the new bound is better replace bestBound and reset candidate set
      if (b < bestBound) {
        bestBound = b
        candidates = ArrayBuffer[Int](x)
      }
        // if the new bound is the same as bestBound add value to candidate set
      else if (b == bestBound) {
        candidates += x
      }
    }
    // select a value randomly from the candidate set
    candidates(rand.nextInt(candidates.length))
  }

  private def bound(i: Int, x: Int): Int = {
    variables(0).context.pushState()
    val out = isInconsistent(variables(0).context.assign(variables(i), x))
    val ret = if(!out) {
      val db = cp.objective.objs.head.domBest
      if(isMinimization) db
      else -db
    }
    else {
      Integer.MAX_VALUE
    }
    variables(0).context.pop()
    ret
  }
}
