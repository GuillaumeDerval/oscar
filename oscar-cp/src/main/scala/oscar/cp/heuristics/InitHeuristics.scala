package oscar.cp.heuristics

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.util.IncrementalStatistics
import oscar.cp._
import oscar.cp.heuristics.HelperFunctions._

import scala.util.Random

class InitHeuristics(solver: CPSolver, decisionVars: Array[CPIntVar], vars:Array[CPIntVar], valH: Int => Int, maxTime:Long, significance:Double) {

  private[this] val context = decisionVars(0).context
  private[this] val nDecVars = decisionVars.length
  private[this] val domSize = Array.tabulate(vars.size)(i => vars(i).size)
  private[this] val decIndices = Array.tabulate(nDecVars)(i => i).toSeq
  private[this] var round = 0
  private[this] val rand = new Random(42)
  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] var objLowerBound = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  private[this] var objUpperBound = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

  // ABS
  private[this] val activity = Array.ofDim[Double](vars.length)
  private[this] val absStats = Array.fill(vars.length)(new IncrementalStatistics())


  // OBS
  private[this] val decDeltaO = Array.ofDim[Double](vars.length)
  private[this] val decObsStats = Array.fill(vars.length)(new IncrementalStatistics())

  private[this] val alpha = if(isMin) -1 else 0
  private[this] val beta = if(isMin) 0 else -1

  private[this] val startTime = System.nanoTime()
  private[this] var current:Long = System.nanoTime()
  private[this] val endTime:Long = current + maxTime

  while(current < endTime && !significanceTest(round)) {
    probe()
    updateStatisticsAndReset()
    current = System.nanoTime()
    round += 1
  }

  for(i <- vars.indices) {
    activity(i) = absStats(i).average
    decDeltaO(i) = decObsStats(i).average
  }


  println("finished probing after: " + round + " rounds and " + (current - startTime).toDouble / 1000000000.0 + " seconds" )


  private def probe(): Unit = {

    solver.pushState()
    val shuffled = rand.shuffle(decIndices)
    var i = 0
    while(i < nDecVars && !context.isFailed) {
      val x = decisionVars(shuffled(i))
      // && !isInconsistent(context.assign(x, valH(shuffled(i)))) && !context.isFailed
      if(!x.isBound && !isInconsistent(solver.assign(x, valH(shuffled(i)))) && !context.isFailed) {
          updateDecisionFeatures()
      }
      i += 1
    }
    solver.pop()

  }


  private def updateDecisionFeatures(): Unit = {

    val objLB = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    val objUB = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

    for(i <- vars.indices) {
      if (vars(i).getSize < domSize(i)) {
        activity(i) += 1
        if(vars(i).isBound) {
          decDeltaO(i) = alpha*Math.abs(objLowerBound - objLB) + beta*Math.abs(objUpperBound - objUB)
        }
      }
      domSize(i) = vars(i).getSize
    }
    objLowerBound = objLB
    objUpperBound = objUB
  }

  private def updateStatisticsAndReset(): Unit = {

    for(i <- vars.indices) {
      absStats(i).addPoint(activity(i))
      activity(i) = 0.0
      decObsStats(i).addPoint(decDeltaO(i))
      decDeltaO(i) = 0.0
      domSize(i) = vars(i).getSize
    }
    // reset objective bounds
    objLowerBound = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    objUpperBound = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  }


  def getAuxiliaryArrays: (Array[Double], Array[Double]) = {
    (activity.slice(decisionVars.length, vars.length), decDeltaO.slice(decisionVars.length, vars.length))
  }


  def getDecisionArrays: (Array[Double], Array[Double]) = {
    (activity.slice(0, decisionVars.length), decDeltaO.slice(0, decisionVars.length))
  }



  private def significanceTest(round:Int): Boolean = {

    // do a minimum number of rounds and only check if the round number is a factor of 11
    if(round < 10 || round % 11 != 0) {
      return false
    }

    for(i <- vars.indices) {
      // ABS test
      var lb = absStats(i).average - student(round) * (Math.sqrt(absStats(i).variance) / Math.sqrt(round))
      var ub = absStats(i).average + student(round) * (Math.sqrt(absStats(i).variance) / Math.sqrt(round))
      if(lb < absStats(i).average*(1-significance) || ub > absStats(i).average*(1+significance)) return false

      // OBS test -> deltaO values can be negative!
      ub = decObsStats(i).average - student(round) * (Math.sqrt(decObsStats(i).variance) / Math.sqrt(round))
      lb = decObsStats(i).average + student(round) * (Math.sqrt(decObsStats(i).variance) / Math.sqrt(round))

      if(lb < decObsStats(i).average*(1-significance) || ub > decObsStats(i).average*(1+significance)) return false
    }

    return true
  }
}
