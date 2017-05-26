package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.search.ALNSStatistics

/**
  * Companion object. Holds the worstTTI current value which is shared among all elements.
  */
object ALNSElement{
  private var worstTTI = 1.0

  def resetWorstTTI(): Unit = { worstTTI = 1.0 }

  private def updateWorstTTI(newTTI: Double) = if(newTTI > worstTTI) worstTTI = newTTI
}

/**
  * This abstract class defines an Adaptive large neighbourhood search element.
  *
  * @param failThreshold The number of failures after which the element should be deactivated.
  */
abstract class ALNSElement(val failThreshold: Int){

  var successfulRuns: Int = 0
  var execs: Int = 0
  var sols: Int = 0
  var time: Long = 0
  var improvement: Int = 0


  protected var nFails = 0 //Number of failures of the element
  private var active: Boolean = true

  def avgTime: Double = if(execs != 0) time.toDouble / execs.toDouble else 0.0

  def avgImprovement: Double = if(execs != 0) improvement.toDouble / execs.toDouble else 0.0

  def successRate: Double = if (execs != 0) successfulRuns.toDouble / execs else 0.0

  def timeToImprovement: Double = {
    if(execs == 0) 0.0
    else if(successRate == 0.0) ALNSElement.worstTTI
    else{
      val tti = (avgTime + 1.0) / successRate
      ALNSElement.updateWorstTTI(tti)
      tti
    }
  }

  /**
    * Updates the element stats and eventual parameters based on the improvement and search statistics given.
    */
  def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    execs += 1
    successfulRuns += Math.min(stats.nSols, 1)
    sols += stats.nSols
    time += stats.time * 1000000
    improvement += costImprovement

    if(fail){
      nFails +=1
      if(failThreshold != 0 && nFails >= failThreshold) setActive(false)
    }
  }

  /**
    * Returns the statistics of the element as an ALNSStatistics object.
    */
  def getStats: ALNSStatistics = new ALNSStatistics(
    execs,
    sols,
    successfulRuns,
    time,
    avgTime,
    improvement,
    avgImprovement,
    successRate,
    timeToImprovement,
    isActive,
    nFails,
    Array[Array[(String, ALNSStatistics)]]()
  )

  /**
    * Activates or deactivates the element
    * @param state boolean indicating if the element should be set active or inactive.
    */
  def setActive(state: Boolean): Unit = {
    active = state
  }

  def resetFails(): Unit = nFails = 0

  def isActive: Boolean = active
}
