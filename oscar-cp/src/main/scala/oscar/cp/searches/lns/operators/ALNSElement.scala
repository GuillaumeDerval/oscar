package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics

import scala.collection.mutable
import scala.xml.{Elem, NodeBuffer}

/**
  * This abstract class defines an Adaptive large neighbourhood search element.
  *
  * @param failThreshold The number of failures after which the element should be deactivated.
  */
abstract class ALNSElement(val failThreshold: Int = 0){

  protected class ExecStats(
                             val tStart: Long,
                             val tEnd: Long,
                             val objStart: Int,
                             val objEnd: Int,
                             val nSols: Int,
                             val searchCompleted: Boolean,
                             val nFails: Int,
                             val iter: Long
                         ){
    def time: Long = tEnd - tStart

    def improvement: Int = Math.abs(objEnd - objStart)

    def isSuccessful: Boolean = nSols > 0
  }

  val stats: mutable.ArrayBuffer[ExecStats] = new mutable.ArrayBuffer[ExecStats]()
  var successfulRuns: Int = 0
  var sols: Int = 0
  var time: Long = 0 //time in nanoseconds
  var improvement: Long = 0
  var lastSuccessIter: Long = 0L

  protected var nFails = 0 //Number of failures of the element
  private var active: Boolean = true

  /**
    * Updates the element stats and eventual parameters based on the improvement and search statistics given.
    */
  def update(
              tStart: Long,
              tEnd: Long,
              objStart: Int,
              objEnd: Int,
              iterStats: SearchStatistics,
              fail: Boolean,
              iter: Long
            ): Unit = {

    val execStats = new ExecStats(tStart, tEnd, objStart, objEnd, iterStats.nSols, iterStats.completed, iterStats.nFails, iter)
    stats += execStats

    if(execStats.isSuccessful){
      successfulRuns += 1
      lastSuccessIter = iter
    }

    sols += execStats.nSols
    time += execStats.time
    improvement += execStats.improvement

    if(fail){
      nFails +=1
      if(failThreshold != 0 && nFails >= failThreshold) setActive(false)
    }
  }

  def lastExecStats: ExecStats = stats.last

  def execs: Int = stats.length

  def efficiency(duration: Long): Double = {
    ???
  }

  def asXml(cat: String): Elem

  //TODO: Print each exec stats
  protected def wrapStatsToXml(): NodeBuffer = {
    <execs>{execs}</execs>
    <sols>{sols}</sols>
    <successful_runs>{successfulRuns}</successful_runs>
    <time>{time}</time>
    <improvement>{improvement}</improvement>
    <state>{if(active) "active" else "inactive"}</state>
    <fails>{nFails}</fails>
  }

  //TODO: Print each exec stats
  override def toString: String = {
    var s = "\texecs: " + execs
    s += "\n\tsols: " + sols
    s += "\n\tsuccessful runs: " + successfulRuns
    s += "\n\ttime(s): " + time / 1000000000.0
    s += "\n\timprovement: " + improvement
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
