package oscar.cp.heuristics

import oscar.cp.{CPIntVar, isInconsistent}
import oscar.cp.core.CPSolver
import oscar.util.IncrementalStatistics

import scala.util.Random

class ObjectiveBasedSelector(solver:CPSolver, variables: Array[CPIntVar], alpha:Int, beta:Int, gamma:Double=0.99) {

  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] var prevLb = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  private[this] var prevUb = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  private[this] val nVars = variables.length
  private[this] val deltaO = Array.fill(nVars)(0.0)
  private[this] val stats = Array.fill(nVars)(new IncrementalStatistics)
  private[this] val rand = new Random(42)
  private[this] val indices = Array.tabulate(nVars)(i => i)
  private[this] val context = variables(0).context
  private[this] var obsMin = Double.MaxValue
  private[this] var obsMax = Double.MinValue
  private[this] val domSize = Array.tabulate(nVars)(i => variables(i).getSize)
  private[this] val studentDistribution = new StudentDistribution
  private[this] val node = variables(0).store

  node.onPush {

    obsMin = Double.MaxValue
    obsMax = Double.MinValue

    val lb =   if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    val ub =  if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

    for(i <- variables.indices) {
      if(variables(i).getSize < domSize(i) && variables(i).isBound) {
        val dO = alpha * math.abs(prevLb - lb) + beta * math.abs(prevUb - ub)
        deltaO(i) = (deltaO(i)*(1-gamma) + gamma*dO)/gamma
      }
      if(deltaO(i) < obsMin) obsMin = deltaO(i)
      else if(deltaO(i) > obsMax) obsMax = deltaO(i)
      domSize(i) = variables(i).getSize
    }

    prevLb = lb
    prevUb = ub
  }

  node.onPop {

    prevLb = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    prevUb = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

    for(i <- variables.indices) {
      domSize(i) = variables(i).getSize
    }
  }



  def probing(maxProbingTime:Long, significance:Double, valH:Int => Int): Unit = {

    var current = System.nanoTime()
    val endTime = current + maxProbingTime
    var probingRound = 0

    while(current < endTime && !statTest(probingRound, significance)) {
      probe(valH)
      updateStatisticsAndReset()
      probingRound += 1
      current = System.nanoTime()
    }

    for(i <- variables.indices) {
      deltaO(i) = stats(i).average
      if(deltaO(i) < obsMin) obsMin = deltaO(i)
      else if (deltaO(i) > obsMax) obsMax = deltaO(i)
    }
  }

  private[this] def probe(valH:Int => Int):Unit = {
    context.pushState()
    val shuffled = rand.shuffle(indices.toSeq)
    var i = 0
    while (i < nVars && !context.isFailed) {
      val x = variables(shuffled(i))
      if (!x.isBound && !isInconsistent(context.assign(x, valH(shuffled(i)))) && !context.isFailed) {
        updateValues()
      }
      i += 1
    }
    context.pop()
  }

  private[this] def updateValues():Unit = {

    val lb =   if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    val ub =  if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

    for(i <- variables.indices) {
      if(variables(i).getSize < domSize(i) && variables(i).isBound) {
        deltaO(i) = alpha * math.abs(prevLb - lb) + beta * math.abs(prevUb - ub)
      }
      domSize(i) = variables(i).getSize
    }

    prevLb = lb
    prevUb = ub

  }

  private[this] def updateStatisticsAndReset():Unit = {
    for(i <- variables.indices) {
      stats(i).addPoint(deltaO(i))
      deltaO(i) = 0
      domSize(i) = variables(i).getSize
    }
    prevLb = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
    prevUb = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest


  }

  private[this] def statTest(currentRound:Int, sig:Double):Boolean = {
    // only check every 5 rounds
    if(currentRound < 10 || currentRound % 5 != 0) {
      return false
    }
    for(i <- variables.indices) {
      val stdDev = Math.sqrt(stats(i).variance / currentRound)
      val lb = stats(i).average - studentDistribution.get(currentRound)*stdDev
      val ub = stats(i).average + studentDistribution.get(currentRound)*stdDev
      if(lb < stats(i).average*(1-sig) || ub > stats(i).average*(1+sig)) return false
    }
    return true
  }


  def getDetlaO(i:Int): Double = {
    deltaO(i)
  }

  def getScaledDeltaO(i:Int): Double = {
    (deltaO(i) - obsMin) / (obsMax - obsMin)
  }

  def setDeltaO(values:Array[Double]): Unit = {

    obsMin = Double.MaxValue
    obsMax = Double.MinValue

    for(i <- values.indices) {
      deltaO(i) = values(i)
      if(deltaO(i) < obsMin) obsMin = deltaO(i)
      else if (deltaO(i) > obsMax) obsMax = deltaO(i)
    }
  }



}
