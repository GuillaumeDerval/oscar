package oscar.cbls.search

import oscar.cbls.objective.Objective
import oscar.cbls.search.core.Neighborhood

object Benchmark extends StopWatch{

  case class RunValues(it:Int,duration:Int,quality:Int)
  case class RunStatistics(it:Statistics,duration:Statistics,quality:Statistics){
    override def toString: String = "(it:" + it + " dur:" + duration + " obj:" + quality+")"
    def denseString:String = it.denseString + "|" + duration.denseString + "|" + quality.denseString
  }

  val firstColumnForStatisticsString = 20
  def nSpace(n:Int):String = if(n <= 0) "" else " " + nSpace(n-1)
  private def padToLength(s: String, l: Int) = (s + nSpace(l)).substring(0, l)
  def benchToStatistics(obj:Objective, nRuns:Int, strategies:(()=>(String,Neighborhood))*) =
    benchToTrace(obj, nRuns, strategies:_*).map{case (s:String,t:IndexedSeq[RunValues]) => (s,aggregate(t.toList))}

  def benchtoString(obj:Objective, nRuns:Int, strategies:(()=>(String,Neighborhood))*):String = {
    val stats = benchToStatistics(obj,nRuns,strategies:_*)

    nSpace(firstColumnForStatisticsString) + "|it                  |dur[ms]             |obj" + "\n" +
      nSpace(firstColumnForStatisticsString) + "|"+Statistics.statisticsHeader + "|" + Statistics.statisticsHeader + "|" + Statistics.statisticsHeader + "\n" +
      stats.map({case (s:String,stats:RunStatistics) => padToLength(s,firstColumnForStatisticsString) + "|" + stats.denseString}).mkString("\n")

  }

  def benchToTrace(obj:Objective, nRuns:Int, strategies:(()=>(String,Neighborhood))*)={
    val m = obj.model
    val initialSolution = m.solution()

    for(n <- strategies)
      yield (n()._1,for(trial <- 0 to nRuns) yield {
        m.restoreSolution(initialSolution)
        val strategyInstance = n()
        this.startWatch()
        val it = strategyInstance._2.doAllMoves(_ => false, obj)
        val time = this.getWatch
        val quality = obj.value
        RunValues(it,time.toInt,quality)
      })
  }

  def aggregate(l:List[RunValues]):RunStatistics = {
    RunStatistics(it=Statistics(l.map(_.it)),
      duration=Statistics(l.map(_.duration)),
      quality=Statistics(l.map(_.quality)))
  }
}

case class Statistics(min:Int, max:Int, avg:Int, med:Int){
  override def toString: String = "(min:" + min + " max:" + max + " avg:" + avg + " med:" + med + ")"
  def denseString:String = padToLength("" + min,4) + " " + padToLength("" + max,4) + " " + padToLength("" + avg,4) + " " + padToLength("" + med,5)
  def nSpace(n:Int):String = if(n <= 0) "" else " " + nSpace(n-1)
  private def padToLength(s: String, l: Int) = (s + nSpace(l)).substring(0, l)
}

object Statistics {
  def apply(l: List[Int]): Statistics = {
    require(l.nonEmpty)
    val sorted = l.sorted
    val size = l.size
    Statistics(min=sorted.head, max = sorted.last, avg=l.sum/size, med=sorted.apply(size/2))
  }

  val statisticsHeader = "min  max  avg  med  "
}
