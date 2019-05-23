package oscar.cp.heuristics

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.util.IncrementalStatistics
import oscar.cp._

import scala.util.Random

class InitHeuristics(solver: CPSolver, variables: Array[CPIntVar], valH: Int => Int, minRounds:Int, maxTime:Long, significance:Double) {

  private[this] val nVariables = variables.length
  private[this] val domSize = Array.tabulate(nVariables)(i => variables(i).size)
  private[this] val indices = Array.tabulate(nVariables)(i => i).toSeq
  private[this] var round = 0
  private[this] val rand = new Random(42)
  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] var objLowerBound = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  private[this] var objUpperBound = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  private[this] val decay = 0.999
  // check if the variable has an impact on the objective bounds during probing
  private[this] val isDecisionVar = Array.tabulate(nVariables)(i => false)


  // ABS
  private[this] val activity = Array.ofDim[Double](nVariables)
  private[this] val absStats = Array.fill(nVariables)(new IncrementalStatistics())

  // OBS
  private[this] val deltaO = Array.ofDim[Double](nVariables)
  private[this] val obsStats = Array.fill(nVariables)(new IncrementalStatistics())
  private[this] val alpha = if(isMin) -1 else 0
  private[this] val beta = if(isMin) 0 else -1

  // WDEG
  private[this] val degree:Array[Double] = Array.tabulate(nVariables)(i => variables(i).constraintDegree)
  private[this] val wdegStats = Array.fill(nVariables)(new IncrementalStatistics())
  private[this] val map = variables.zipWithIndex.map(y => (y._1.asInstanceOf[CPVar], y._2)).toMap

  // values of the student distribution for a 95% confidence interval
  private[this] val distribution = Array(
    999.99,
    12.706, 4.303, 3.182, 2.776, 2.571, // 1...5
    2.447, 2.365, 2.306, 2.262, 2.228,  // 6...10
    2.201, 2.179, 2.160, 2.145, 2.131,  // 10...15
    2.120, 2.110, 2.101, 2.093, 2.086,  // 16...20
    2.080, 2.074, 2.069, 2.064, 2.060,  // 21...25
    2.056, 2.052, 2.048, 2.045, 2.042,  // 26...30
    2.040, 2.037, 2.035, 2.032, 2.030,  // 31...35
    2.028, 2.026, 2.024, 2.023, 2.021,  // 36...40
    2.000, 1.990, 1.984, 1.980, 1.977,  // 60, 80, 100, 120, 140
    1.975, 1.973, 1.972, 1.969, 1.960   // 160, 180, 200, 250, inf
  )

  private[this] def student(index:Int):Double = {
    if(index < 41) distribution(index-1)
    else if (index < 61) distribution(40)
    else if (index < 81) distribution(41)
    else if (index < 101) distribution(42)
    else if(index < 121) distribution(43)
    else if (index < 141) distribution(44)
    else if (index < 161) distribution(45)
    else if (index < 181) distribution(46)
    else if (index < 201) distribution(47)
    else if (index < 251) distribution(48)
    else distribution(49)
  }


  private def updateWdeg():Unit = {

    for(i <- variables.indices) {
      degree(i) *= decay
    }
    if(solver.lastConstraintCalled != null) {
      for (v <- solver.lastConstraintCalled.associatedVars()) {
        val index = map.getOrElse(v, -1)

        if (index > 0) {
          degree(index) += 1
        }
      }
    }
  }

  solver.onFailure(updateWdeg())


  private[this] val startTime = System.nanoTime()
  private[this] var current:Long = System.nanoTime()
  private[this] val endTime:Long = current + maxTime

  while(current < endTime && !significanceTest(round)) {
    probe()
    updateStatisticsAndReset()
    current = System.nanoTime()
    round += 1
  }

  for(i <- variables.indices) {
    activity(i) = absStats(i).average
    deltaO(i) = obsStats(i).average
    degree(i) = wdegStats(i).average
  }

  println("finished probing after: " + round + " rounds and " + (current - startTime).toDouble / 1000000000.0 + " seconds" )


  val tmp = deltaO.filter(_ != 0.0)


  private def probe(): Unit = {
    solver.pushState()
    val shuffled = rand.shuffle(indices)
    var i = 0
    while(i < nVariables) {
      val x = variables(shuffled(i))
      // && !isInconsistent(context.assign(x, valH(shuffled(i)))) && !context.isFailed
      if(!x.isBound && !isInconsistent(solver.assign(x, valH(shuffled(i))))) {
        val objLB = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
        val objUB = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
        // the variable has an inpact on the objective bounds
        if(objLB != objLowerBound || objUB != objUpperBound) {
          isDecisionVar(shuffled(i)) = true
        }
        updateActivities()
      }
      i += 1
    }
    solver.pop()
  }


  private def updateActivities(): Unit = {

    if(solver.isFailed) {
      return
    }
    val objLB = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    val objUB = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

    for(i <- variables.indices) {
      if (variables(i).getSize < domSize(i)) {
        activity(i) += 1
        if(variables(i).isBound) {
          deltaO(i) = alpha*Math.abs(objLowerBound - objLB) + beta*Math.abs(objUpperBound - objUB)

        }
      }
      domSize(i) = variables(i).getSize
    }

    objLowerBound = objLB
    objUpperBound = objUB
  }

  private def updateStatisticsAndReset(): Unit = {

    for(i <- variables.indices) {

      absStats(i).addPoint(activity(i))
      activity(i) = 0.0

      obsStats(i).addPoint(deltaO(i))
      deltaO(i) = 0.0

      wdegStats(i).addPoint(degree(i))
      degree(i) = variables(i).constraintDegree

      domSize(i) = variables(i).getSize
    }

    // reset objective bounds
    objLowerBound = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    objUpperBound = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  }


  def getFeatureArrays: (Array[Double], Array[Double], Array[Double]) = {
    (activity, deltaO, degree)
  }

  def getDecisionVars: Array[Boolean] = {
    isDecisionVar
  }


  private def significanceTest(round:Int): Boolean = {

    // do a minimum number of rounds and only check if the round number is a factor of 5
    if(round < minRounds || round % 5 != 0) {
      return false
    }

    for(i <- variables.indices) {

      // ABS test
      var lb = absStats(i).average - student(round) * (Math.sqrt(absStats(i).variance) / Math.sqrt(round))
      var ub = absStats(i).average + student(round) * (Math.sqrt(absStats(i).variance) / Math.sqrt(round))
      if(lb < absStats(i).average*(1-significance) || ub > absStats(i).average*(1+significance)) return false

      // OBS test -> deltaO values can be negative!
      val b1 = obsStats(i).average - student(round) * (Math.sqrt(obsStats(i).variance) / Math.sqrt(round))
      val b2 = obsStats(i).average + student(round) * (Math.sqrt(obsStats(i).variance) / Math.sqrt(round))
      if(b1 < b2) {
        lb = b1
        ub = b2
      }
      else {
        lb = b2
        ub = b1
      }
      if(lb < obsStats(i).average*(1-significance) || ub > obsStats(i).average*(1+significance)) return false

      // WDEG test
      lb = wdegStats(i).average - student(round) * (Math.sqrt(wdegStats(i).variance) / Math.sqrt(round))
      ub = obsStats(i).average + student(round) * (Math.sqrt(wdegStats(i).variance) / Math.sqrt(round))
      if(lb < wdegStats(i).average*(1-significance) || ub > wdegStats(i).average*(1+significance)) return false

    }
    return true

  }



}
