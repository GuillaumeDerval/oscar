package oscar.algo.search


import scala.util.Random
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleBoolean

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class DFSearchNode extends ReversibleContext {

  var silent = false

  val random: Random = new Random(0)

  /**
    * @return The Random generator of this node potentially used in other algorithms
    */
  def getRandom(): Random = random

  protected val failed = new ReversibleBoolean(this, false)

  protected val searchStrategy = new DFSearch(this)

  protected var heuristic: Branching = null

  final def searchEngine: DFSearch = searchStrategy

  final def onSolution(action: => Unit)(implicit listener: DFSearchListener): DFSearchNode = {
    listener.onSolution(action); this
  }

  /** @return  true if this node can surely not lead to any solution */
  def isFailed(): Boolean = failed.value

  /** Set the node in a failed state */
  def fail(): Unit = failed.setTrue()
  
  /** This function is executed when the node becomes a solution */
  def solFound(): Unit = Unit

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit listener: DFSearchListener = new DefaultDFSearchListener()): SearchStatistics = {
    startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)()(listener)
  }

  /*def start(stopCondition: => Boolean): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue, null)(Unit)
  }*/

  def start(stopCondition: => Boolean)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue)(Unit)
  }

  /*def start(stopCondition: => Boolean, maxDiscrepancy: Int): SearchStatistics = {
    startSubjectTo(stopCondition, maxDiscrepancy, null)(Unit)
  }*/

  def start(stopCondition: => Boolean, maxDiscrepancy: Int)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, maxDiscrepancy)(Unit)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(block: => Unit = Unit)(implicit listener: DFSearchListener = DefaultDFSearchListener()): SearchStatistics = {
    val stopCondition = buildStopCondition(nSols, failureLimit, timeLimit)
    startSubjectTo(stopCondition, maxDiscrepancy)(block)
  }

  /*def startSubjectTo(stopCondition: => Boolean)(block: => Unit)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue, null)(block)
  }*/

  def startSubjectTo(stopCondition: => Boolean)(block: => Unit)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue)(block)
  }

  /*def startSubjectTo(stopCondition: => Boolean, maxDiscrepancy: Int)(block: => Unit)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo((s: DFSearch) => stopCondition, maxDiscrepancy, null)(block)
  }*/

  def startSubjectTo(stopCondition: => Boolean, maxDiscrepancy: Int)(block: => Unit)(implicit listener: DFSearchListener): SearchStatistics = {
    startSubjectTo((s: DFSearch) => stopCondition, maxDiscrepancy)(block)(listener)
  }

  def startSubjectTo(stopCondition: DFSearch => Boolean, maxDiscrepancy: Int)(block: => Unit)(implicit listener: DFSearchListener): SearchStatistics = {
    val t0 = System.currentTimeMillis()
    pushState() // Store the current state
    block // Apply the before search action
    searchStrategy.start(heuristic.maxDiscrepancy(maxDiscrepancy), stopCondition)
    pop() // Restore the current state
    // Build the statistic object
    new SearchStatistics(
      searchStrategy.nNodes,
      searchStrategy.nBacktracks,
      System.currentTimeMillis() - t0,
      searchStrategy.isCompleted,
      this.time,
      this.maxSize,
      searchStrategy.nSolutions
    )
  }

  @inline private def buildStopCondition(nSols: Int, failureLimit: Int, timeLimit: Int): Function1[DFSearch, Boolean] = {
    // Build the stop condition
    val checkSol = nSols < Int.MaxValue
    val checkFailures = failureLimit < Int.MaxValue
    val checkTime = timeLimit < Int.MaxValue
    val maxTime = (timeLimit * 1000) + System.currentTimeMillis()
    (s: DFSearch) => {
      var stop = false
      stop |= (checkSol && s.nSolutions >= nSols)
      stop |= (checkFailures && s.nBacktracks >= failureLimit)
      stop |= (checkTime && System.currentTimeMillis() >= maxTime)
      stop
    }
  }

  def search(block: => Seq[Alternative]): DFSearchNode = {
    heuristic = Branching(block); this
  }

  def search(branching: Branching): DFSearchNode = {
    heuristic = branching; this
  }
}