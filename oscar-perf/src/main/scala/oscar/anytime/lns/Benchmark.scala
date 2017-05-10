package oscar.anytime.lns

import oscar.anytime.lns.utils.XmlWriter
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}

trait Benchmark {

  def decisionVariables: Array[CPIntVar]
  def objective: CPIntVar
  def bestKnownObjective: Int

  def main(args: Array[String]): Unit = {
    val cp = objective.store
    val timeout = args(0).toLong * 1000000000L

    val alns = ALNSSearch(
      decisionVariables,
      objective,
      new ALNSConfig(
        timeout = timeout,
        coupled = true,
        learning = false,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
        Array(
          ALNSBuilder.ConfOrder,
          ALNSBuilder.FirstFail,
          ALNSBuilder.LastConf,
          ALNSBuilder.BinSplit,
          ALNSBuilder.ConfOrderValLearn,
          ALNSBuilder.FirstFailValLearn,
          ALNSBuilder.LastConfValLearn,
          ALNSBuilder.BinSplitValLearn
        ),
        ALNSBuilder.Priority,
        ALNSBuilder.RWheel,
        ALNSBuilder.TTI,
        ALNSBuilder.TTI
      )
    )

    val result = alns.search()

    XmlWriter.writeToXml("../ALNS-bench-results/", "testMethod", "testInstance", bestKnownObjective, maxObjective = false, result.solutions)
  }

}

