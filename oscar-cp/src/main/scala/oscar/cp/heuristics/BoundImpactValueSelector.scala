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
  * @param maxDomSize Limits the number of values that are considered during the selection process, by default 50
  * @param tiebreaker A secondary value heuristic that can be used as a tiebreaker, by default no tiebreaker is used
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class BoundImpactValueSelector(cp: CPSolver, variables: Array[CPIntVar], maxDomSize: Int= 50, tiebreaker: (Int, Int) => Int = (i, j) => Integer.MAX_VALUE) {

  private[this] val isMinimization:Boolean = cp.objective.objs.head.isMin
  private[this] val threshold = maxDomSize
  private[this] val rand = new Random(42)

  def selectValue(i: Int): Int = {

    var bestBound = bound(i, variables(i).getMin)
    var bestValue = variables(i).getMin
    var tieBreakScore = Integer.MAX_VALUE
    var candidates = ArrayBuffer[Int]()

    val values = new Array[Int](variables(i).getSize)
    variables(i).fillArray(values)

    val vSeq = values.toSeq
    val shuffled = rand.shuffle(vSeq)
    val maxCount = if(values.length > threshold) threshold else values.length

    for(j <- 0 until maxCount) {
      val x = shuffled(j)
      val b = bound(i, x)
      if (b < bestBound) {
        bestValue = x
        bestBound = b
        tieBreakScore = tiebreaker(i, x)
        candidates = ArrayBuffer[Int](x)
      }
      else if (b == bestBound) {
        if(tiebreaker(i, x) < tieBreakScore) {
          bestValue = x
          candidates = ArrayBuffer[Int](x)
          tieBreakScore = tiebreaker(i, x)
        }
        else if(tiebreaker(i, x) == tieBreakScore) {
          candidates += x
        }
      }
    }
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
