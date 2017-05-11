package oscar.anytime.lns.models

import oscar.anytime.lns.utils.XmlWriter
import oscar.cp.searches.lns.search.ALNSConfig
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.searches.alns.operators.ALNSBuilder
import oscar.modeling.solvers.cp.searches.alns.search.ALNSSearch
import oscar.xcsp3.XCSP3Parser2

class XCSP(file: String){
  val md = new ModelDeclaration

  def main(args: Array[String]): Unit = {
    val timeout = args(0).toLong * 1000000000L

    //Parsing the instance and instantiating model declaration
    val (vars, solutionGenerator) = XCSP3Parser2.parse(md, file)

    val alns = ALNSSearch(
      md,
      vars,
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
      ),
      solutionGenerator
    )

    val result = alns.search()

    XmlWriter.writeToXml("../ALNS-bench-results/", "default", "test", Int.MaxValue, maxObjective = false, result.solutions)
  }

}
