package oscar.cp.searches.lns.selection

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.operators.ALNSElement

object Metrics {

  def lastImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    improvement.toDouble / (stats.time * 1000000 + 1).toDouble
  }

  def averageImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.avgImprovement
  }

  def timeToImprovement(element: ALNSElement, improvement: Int, stats: SearchStatistics): Double = {
    element.timeToImprovement
  }
}
