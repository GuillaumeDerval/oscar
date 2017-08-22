package oscar.cp.searches.lns.selection

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.operators.ALNSElement

object Metrics {

  def lastImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    improvement.toDouble.toDouble
  }

  def lastImprovementRatio(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    improvement.toDouble / (stats.time + 1)
  }

  def averageImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.avgImprovement
  }

  def averageImprovementRatio(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.improvement / (element.time / 1000000.0 + 1)
  }

  def timeToImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.timeToImprovement
  }
}
