package oscar.cp.heuristics

import oscar.cp.{CPIntVar, isInconsistent}
import oscar.cp.core.CPSolver
import oscar.util.IncrementalStatistics
import oscar.cp.heuristics.HelperFunctions._
import scala.util.Random

class ObjectiveBasedSelector(solver:CPSolver, variables: Array[CPIntVar], alpha:Int, beta:Int, gamma:Double=0.4) {

  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] var prevLb = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  private[this] var prevUb = if(isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  private[this] val nVars = variables.length
  private[this] val deltaO = Array.fill(nVars)(0.0)
  private[this] val stats = Array.fill(nVars)(new IncrementalStatistics)
  private[this] val rand = new Random(42)
  private[this] val indices = Array.tabulate(nVars)(i => i)
  private[this] var obsMin = Double.MaxValue
  private[this] var obsMax = Double.MinValue
  private[this] val domSize = Array.tabulate(nVars)(i => variables(i).getSize)


  if(variables.nonEmpty) {

    val node = variables(0).store
    node.onPush {

      obsMin = Double.MaxValue
      obsMax = Double.MinValue

      val lb = if (isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
      val ub = if (isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

      for (i <- variables.indices) {
        if (variables(i).getSize < domSize(i) && variables(i).isBound) {
          val dO = alpha * math.abs(prevLb - lb) + beta * math.abs(prevUb - ub)
          if(deltaO(i) == Double.MinValue) {
            deltaO(i) = dO
          }
          else {
            deltaO(i) = deltaO(i) * (1 - gamma) + gamma * dO
          }
        }
        if (deltaO(i) < obsMin) obsMin = deltaO(i)
        if (deltaO(i) > obsMax) obsMax = deltaO(i)
        domSize(i) = variables(i).getSize
      }

      prevLb = lb
      prevUb = ub
    }

    node.onPop {
      prevLb = if (isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
      prevUb = if (isMin) solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
      for (i <- variables.indices) {
        domSize(i) = variables(i).getSize
      }
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
      if (deltaO(i) > obsMax) obsMax = deltaO(i)
    }
  }

  private[this] def probe(valH:Int => Int):Unit = {
    variables(0).context.pushState()
    val shuffled = rand.shuffle(indices.toSeq)
    var i = 0
    while (i < nVars && !variables(0).context.isFailed) {
      val x = variables(shuffled(i))
      if (!x.isBound && !isInconsistent(variables(0).context.assign(x, valH(shuffled(i)))) && !variables(0).context.isFailed) {
        updateValues()
      }
      i += 1
    }
    variables(0).context.pop()
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
      val ub = stats(i).average - student(currentRound)*stdDev
      val lb = stats(i).average + student(currentRound)*stdDev
      if(lb < stats(i).average*(1-sig) || ub > stats(i).average*(1+sig)) return false
    }
    true
  }


  def getDeltaO(i:Int): Double = {
    deltaO(i)
  }

  def getOppositeDeltaO(i:Int):Double = {
    -deltaO(i)
  }

  def getScaledDeltaO(i:Int): Double = {
    val res = if (obsMin == obsMax) {
      0
    }
    else {
      (deltaO(i) - obsMin) / (obsMax - obsMin)
    }
    // TODO REMOVE FOR FINAL COMPETATION
    if(res > 1 || res < 0) {
      println("ERROR -> DELTA 0 not in range")
      println("deltaO: " + deltaO(i))
      println("obsMin : " + obsMin)
      println("obsMax : " + obsMax)
    }
    res
  }

  def setDeltaO(values:Array[Double]): Unit = {
    obsMin = Double.MaxValue
    obsMax = Double.MinValue

    for(i <- values.indices) {
      deltaO(i) = values(i)
      if(deltaO(i) < obsMin) obsMin = deltaO(i)
      if (deltaO(i) > obsMax) obsMax = deltaO(i)
    }
  }


}
