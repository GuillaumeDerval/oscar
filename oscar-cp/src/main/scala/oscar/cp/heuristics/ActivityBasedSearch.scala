package oscar.cp.heuristics

import oscar.cp.CPIntVar
import oscar.cp._
import oscar.util.IncrementalStatistics
import scala.util.Random
import oscar.cp.heuristics.HelperFunctions._

class ActivityBasedSearch(variables: Array[CPIntVar], decay:Double=0.999) {

  private[this] val nVars = variables.length
  private[this] val activity = Array.fill(nVars)(0.0)
  private[this] val stats = Array.fill(nVars)(new IncrementalStatistics)
  private[this] val rand = new Random(42)
  private[this] val indices = Array.tabulate(nVars)(i => i)
  private[this] val activityOverDom = Array.fill(nVars)(0.0)
  private[this] var absMin = Double.MaxValue
  private[this] var absMax = Double.MinValue
  private[this] val domSize = Array.tabulate(nVars)(i => variables(i).getSize)


  if(variables.nonEmpty) {

    val node = variables(0).store

    node.onPush {

      absMin = Double.MaxValue
      absMax = Double.MinValue

      for(i <- variables.indices) {

        if(variables(i).getSize < domSize(i)) {
          activity(i) += 1
        }
        else if (!variables(i).isBound) {
          activity(i) *= decay
        }

        activityOverDom(i) = activity(i)/variables(i).getSize
        if(activityOverDom(i) < absMin) absMin = activityOverDom(i)
        if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
        domSize(i) = variables(i).getSize
      }
    }

    node.onPop {
      for(i <- variables.indices) {
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
      activity(i) = stats(i).average
      activityOverDom(i) = activity(i)/variables(i).getSize
      if(activityOverDom(i) < absMin) absMin = activityOverDom(i)
      if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
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

    for(i <- variables.indices) {
      if(variables(i).getSize < domSize(i)) {
        activity(i) += 1
      }
      domSize(i) = variables(i).getSize
    }
  }

  private[this] def updateStatisticsAndReset():Unit = {
    for(i <- variables.indices) {
      stats(i).addPoint(activity(i))
      activity(i) = 0
      domSize(i) = variables(i).getSize
    }
  }

  private[this] def statTest(currentRound:Int, sig:Double):Boolean = {
    // only check every 5 rounds
    if(currentRound < 10 || currentRound % 5 != 0) {
      return false
    }
    for(i <- variables.indices) {
      val stdDev = Math.sqrt(stats(i).variance / currentRound)
      val lb = stats(i).average - student(currentRound)*stdDev
      val ub = stats(i).average + student(currentRound)*stdDev
      if(lb < stats(i).average*(1-sig) || ub > stats(i).average*(1+sig)) return false
    }
    true
  }


  def getActivity(i:Int): Double = {
    activity(i)
  }

  def getActivityOverDom(i:Int):Double = {
    activityOverDom(i)
  }

  // returns the scaled value of activity/dom
  def getScaledActivity(i:Int): Double = {
    val res = if (absMin == absMax) {
      0
    }
    else {
      (activityOverDom(i) - absMin) / (absMax - absMin)
    }
    // TODO: remove for the xcsp3 competition
    if(res > 1 || res < 0) {
      println("ERROR -> ABS not in range [0, 1]")
      println("abs: " + activityOverDom(i))
      println("absMin : " + absMin)
      println("obsMax : " + absMax)
    }
    res
  }

  // set the activity values from another array
  def setActivity(values:Array[Double]): Unit = {
    absMin = Double.MaxValue
    absMax = Double.MinValue
    for(i <- values.indices) {
      activity(i) = values(i)
      activityOverDom(i) = activity(i)/variables(i).getSize
      if(activityOverDom(i) < absMin) absMin = activityOverDom(i)
      if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
    }
  }

}
