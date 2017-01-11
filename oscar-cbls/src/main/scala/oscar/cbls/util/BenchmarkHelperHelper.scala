package oscar.cbls.search

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

/*
import oscar.cbls.search.core.Neighborhood
import oscar.cbls.search.Benchmark.benchToStatistics
import oscar.cbls.routing.model.VRPObjective

object BenchmarkHelperHelper {

  def benchmark[T <: VRPObjective](problems: Iterable[() => (String, T)], nRuns: Int, strategies: Iterable[T => Neighborhood],
                                   warmupTimeInSeconds: Int = 10, verbose: Int = 0) = {
    val statistics = problems map (problem => {
      val (name, vrp) = problem()
      println("Benchmarking with problem " + name)
      (name, benchToStatistics(vrp.getObjective, nRuns,
        strategies.map(s => () => {
          val strategy = s(vrp)
          (strategy.toString, strategy)
        }), warmupTimeInSeconds,
        verbose))
    })
    println("nRuns = " + nRuns)
    statistics foreach {
      case (problemPath: String, s: Iterable[(String, Benchmark.RunStatistics)]) =>
        val problemName = problemPath.substring(problemPath.lastIndexOf("/") + 1)
        println
        println(problemName)
        println("strategy;it;dur[ms];obj")
        println(statsToCsv(s))
//        File("dataSample/csv_results/" + problemName + "_" + System.currentTimeMillis / 1000 + ".csv").writeAll("strategy;it;dur[ms];obj\n" + statsToCsv(s))
    }
  }

  def statsToCsv(stats: Iterable[(String, Benchmark.RunStatistics)], nRuns: Int = 1) = {
    stats.map({
      case (s: String, stats: Benchmark.RunStatistics) =>
        if (nRuns > 1)
          List(s, stats.it.min, stats.duration.min, stats.quality.min).mkString(";")
        else
          List(s, stats.it.min, stats.duration.min, stats.quality.min).mkString(";")
    }).mkString("\n")
  }
}
*/