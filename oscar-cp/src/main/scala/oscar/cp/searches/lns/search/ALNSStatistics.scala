package oscar.cp.searches.lns.search

import scala.xml.Elem

/**
  * ALNS statistics
  */
class ALNSStatistics(
                    val execs: Int,
                    val sols: Int,
                    val successfulRuns: Int,
                    val time: Long,
                    val avgTime: Double,
                    val improvement: Int,
                    val avgImprovement: Double,
                    val successRate: Double,
                    val timeToImprovement: Double,
                    val active: Boolean,
                    val fails: Int,
                    val paramStats: Array[Array[(String, ALNSStatistics)]]
               ){


  def asXml(name: String, opType:String): Elem = {
    <operator>
      <name>{name}</name>
      <type>{opType}</type>
      <execs>{execs}</execs>
      <sols>{sols}</sols>
      <successful_runs>{successfulRuns}</successful_runs>
      <time>{time}</time>
      <avg_time>{avgTime}</avg_time>
      <improvement>{improvement}</improvement>
      <avg_improvement>{avgImprovement}</avg_improvement>
      <success_rate>{successRate}</success_rate>
      <time_to_improvement>{timeToImprovement}</time_to_improvement>
      <state>{if(active) "active" else "inactive"}</state>
      <fails>{fails}</fails>
    </operator>
  }

  override def toString: String = {
    var s = "statistics: "
    s += "\n\texecs: " + execs
    s += "\n\tsols: " + sols
    s += "\n\tsuccessful runs: " + successfulRuns
    s += "\n\ttime: " + time
    s += "\n\taverage time: " + avgTime
    s += "\n\timprovement: " + improvement
    s += "\n\taverage improvement: " + avgImprovement
    s += "\n\tsuccess rate: " + successRate
    s += "\n\ttime to improvement: " + timeToImprovement
    s += "\n\tstatus: " + (if(active) "active" else "inactive")
    s += "\n\tfails: " + fails
    s
  }
}
