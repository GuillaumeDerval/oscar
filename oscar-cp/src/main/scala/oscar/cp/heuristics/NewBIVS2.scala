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
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class NewBIVS2(cp: CPSolver, variables: Array[CPIntVar], maxDomSize: Int= 20) {

  private[this] val isMinimization:Boolean = cp.objective.objs.head.isMin
  private[this] val threshold = maxDomSize
  private[this] val rand = new Random(42)

  def selectValue(i: Int): Int = {

    var bestBound = Integer.MAX_VALUE
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
        bestBound = b
        candidates = ArrayBuffer[Int](x)
      }
      else if (b == bestBound) {
        candidates += x
      }
    }
    candidates(rand.nextInt(candidates.length))
  }


  private def bound(i: Int, x: Int): Int = {
    variables(0).context.pushState()
    val out = isInconsistent(variables(0).context.assign(variables(i), x))
    val ret = if(!out) {
      val newDw = cp.objective.objs.head.domWorst
      val newDb = cp.objective.objs.head.domBest
      if (isMinimization) {
        newDb + newDw
      }
      else {
          -newDb - newDw
      }
    }
    else {
      Integer.MAX_VALUE
    }
    variables(0).context.pop()
    ret
  }
}
