package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics

import scala.xml.{Elem, NodeBuffer}

/**
  * This abstract class defines an Adaptive large neighbourhood search element.
  *
  * @param failThreshold The number of failures after which the element should be deactivated.
  */
abstract class ALNSElement(
                            val perfMetric: (ALNSElement, Int, SearchStatistics) => Double,
                            var score: Double = 0.0,
                            var rFactor: Double = 1.0,
                            val failThreshold: Int = 0
                          ){

  var successfulRuns: Int = 0
  var execs: Int = 0
  var sols: Int = 0
  var time: Long = 0 //time in nanoseconds
  var improvement: Long = 0
  var lastSucessIter: Long = 0L

  protected var nFails = 0 //Number of failures of the element
  private var active: Boolean = true

  /**
    * Updates the element stats and eventual parameters based on the improvement and search statistics given.
    */
  def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean, iter: Long): Unit = {
    if(stats.nSols > 0){
      successfulRuns += 1
      lastSucessIter = iter
    }
    sols += stats.nSols
    time += stats.time * 1000000
    improvement += costImprovement

    val perf = perfMetric(this, costImprovement, stats)
    if(execs > 0) score = rFactor * perf + (1.0 - rFactor) * score
    else score = perf

    if(fail){
      nFails +=1
      if(failThreshold != 0 && nFails >= failThreshold) setActive(false)
    }

    execs += 1
  }

  def asXml(cat: String): Elem

  protected def wrapStatsToXml(): NodeBuffer = {
    <execs>{execs}</execs>
    <sols>{sols}</sols>
    <successful_runs>{successfulRuns}</successful_runs>
    <time>{time}</time>
    <improvement>{improvement}</improvement>
    <score>{score}</score>
    <state>{if(active) "active" else "inactive"}</state>
    <fails>{nFails}</fails>
  }

  override def toString: String = {
    var s = "\texecs: " + execs
    s += "\n\tsols: " + sols
    s += "\n\tsuccessful runs: " + successfulRuns
    s += "\n\ttime(s): " + time / 1000000000.0
    s += "\n\timprovement: " + improvement
    s += "\n\tscore: " + score
    s += "\n\tstatus: " + (if(active) "active" else "inactive")
    s += "\n\tfails: " + nFails
    s
  }

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
