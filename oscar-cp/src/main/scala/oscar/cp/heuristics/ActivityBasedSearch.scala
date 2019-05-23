package oscar.cp.heuristics

import oscar.cp.CPIntVar
import oscar.cp._
import oscar.util.IncrementalStatistics
import scala.util.Random

class ActivityBasedSearch(variables: Array[CPIntVar], decay:Double) {

  private[this] val nVars = variables.length
  private[this] val activity = Array.fill(nVars)(0.0)
  private[this] val stats = Array.fill(nVars)(new IncrementalStatistics)
  private[this] val rand = new Random(42)
  private[this] val indices = Array.tabulate(nVars)(i => i)
  private[this] val context = variables(0).context
  private[this] val activityOverDom = Array.fill(nVars)(0.0)
  private[this] var absMin = Double.MaxValue
  private[this] var absMax = Double.MinValue
  private[this] val domSize = Array.tabulate(nVars)(i => variables(i).getSize)
  private[this] val studentDistribution = new StudentDistribution
  private[this] val node = variables(0).store

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
      else if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
      domSize(i) = variables(i).getSize
    }

  }

  node.onPop {
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
      activity(i) = stats(i).average
      activityOverDom(i) = activity(i)/variables(i).getSize
      if(activityOverDom(i) < absMin) absMin = activityOverDom(i)
      else if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
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
      val lb = stats(i).average - studentDistribution.get(currentRound)*stdDev
      val ub = stats(i).average + studentDistribution.get(currentRound)*stdDev
      if(lb < stats(i).average*(1-sig) || ub > stats(i).average*(1+sig)) return false
    }
    return true
  }


  def getActivity(i:Int): Double = {
    activity(i)
  }

  def getActivityOverDom(i:Int):Double = {
    activityOverDom(i)
  }

  def getScaledActivity(i:Int): Double = {
    (activityOverDom(i) - absMin) / (absMax - absMin)
  }

  def setActivity(values:Array[Double]): Unit = {

    absMin = Double.MaxValue
    absMax = Double.MinValue

    for(i <- values.indices) {
      activity(i) = values(i)
      activityOverDom(i) = activity(i)/variables(i).getSize
      if(activityOverDom(i) < absMin) absMin = activityOverDom(i)
      else if(activityOverDom(i) > absMax) absMax = activityOverDom(i)
    }
  }

}
