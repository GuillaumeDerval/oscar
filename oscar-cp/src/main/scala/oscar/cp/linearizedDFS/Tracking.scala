package oscar.cp.linearizedDFS

import java.lang.management.ManagementFactory
import oscar.cp.core.CPOutcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint


/**
 * Created by saschavancauwelaert on 14/11/14.
 */
trait Tracking extends Constraint {

  val decisionVariables : Seq[CPIntVar]

  var numUselessCalls = 0 //call with no pruning
  var numUsefulCalls = 0 //call with some pruning (at least one value)

  var timeSpentOnUsefulCalls = 0.0
  var timeSpentOnUselessCalls = 0.0

  var currentAverageTimeSpentOnUselessCalls = 0.0
  var currentAverageTimeSpentOnUsefulCalls = 0.0
  //BIASED sample variances
  var currentVarianceTimeSpentOnUselessCalls = 0.0
  var currentVarianceTimeSpentOnUsefulCalls = 0.0

  protected val timeThreadBean = ManagementFactory.getThreadMXBean()
  protected val numDecisionVariables = decisionVariables.size
  protected val oldDomainSizes = Array.fill(numDecisionVariables)(-1)
  protected var i = 0
  protected var pruningOccurred = false
  protected var timeBeforePropagate = 0.0
  protected var propagationTime = 0.0

  override def propagate() : CPOutcome = {

    pruningOccurred = false
    //keep domain sizes before propagate
    i = 0
    while(i<numDecisionVariables) {
      oldDomainSizes(i)=decisionVariables(i).size
      i += 1
    }
    timeBeforePropagate = timeThreadBean.getCurrentThreadUserTime
    val outcome = super.propagate()
    propagationTime = timeThreadBean.getCurrentThreadUserTime - timeBeforePropagate

    //check if pruning occurred
    i = 0
    while(!pruningOccurred && i<numDecisionVariables) {
      if(decisionVariables(i).size != oldDomainSizes(i))
        pruningOccurred = true
      i += 1
    }
    if(pruningOccurred) {
      currentVarianceTimeSpentOnUsefulCalls = (currentVarianceTimeSpentOnUsefulCalls + currentAverageTimeSpentOnUsefulCalls * currentAverageTimeSpentOnUsefulCalls) * (numUsefulCalls)
      numUsefulCalls+= 1
      timeSpentOnUsefulCalls += propagationTime
      currentAverageTimeSpentOnUsefulCalls = (currentAverageTimeSpentOnUsefulCalls * (numUsefulCalls - 1) + propagationTime) / numUsefulCalls
      currentVarianceTimeSpentOnUsefulCalls = (currentVarianceTimeSpentOnUsefulCalls + propagationTime * propagationTime)/numUsefulCalls - currentAverageTimeSpentOnUsefulCalls * currentAverageTimeSpentOnUsefulCalls
    }
    else {
      currentVarianceTimeSpentOnUselessCalls = (currentVarianceTimeSpentOnUselessCalls + currentAverageTimeSpentOnUselessCalls * currentAverageTimeSpentOnUselessCalls) * (numUselessCalls)
      numUselessCalls += 1
      timeSpentOnUselessCalls += propagationTime
      currentAverageTimeSpentOnUselessCalls = (currentAverageTimeSpentOnUselessCalls * (numUselessCalls - 1) + propagationTime) / numUselessCalls
      currentVarianceTimeSpentOnUselessCalls = (currentVarianceTimeSpentOnUselessCalls + propagationTime * propagationTime)/numUselessCalls - currentAverageTimeSpentOnUselessCalls * currentAverageTimeSpentOnUselessCalls
    }
    outcome
  }

}
