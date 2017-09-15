package oscar.cp.searches.lns.selection

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.operators.ALNSElement

object Metrics {
  var worstTTI = 1.0

  def avgTime(element: ALNSElement): Double = if(element.execs != 0) element.time / element.execs else 0.0

  def avgImprovement(element: ALNSElement): Double = if(element.execs != 0) element.improvement / element.execs else 0.0

  def successRate(element: ALNSElement): Double = if (element.execs != 0) element.successfulRuns / element.execs else 0.0

  def timeToImprovement(element: ALNSElement): Double = {
    if(element.execs == 0) 0.0
    else if(successRate(element) == 0.0) worstTTI
    else{
      val tti = (avgTime(element) + 1.0) / successRate(element)
      if(tti > worstTTI) worstTTI = tti
      tti
    }
  }

  def lastImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    improvement.toDouble
  }

  def lastImprovementRatio(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    improvement.toDouble / (stats.time + 1)
  }

  def averageImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    avgImprovement(element)
  }

  def averageImprovementRatio(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.improvement / (element.time / 1000000.0 + 1)
  }

  def timeToImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    timeToImprovement(element)
  }
}
