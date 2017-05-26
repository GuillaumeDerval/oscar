package oscar.xcsp3.competition.solvers

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.xcsp3.XCSP3Parser
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.util.Random

class ALNSSolver extends CompetitionApp{

  override def runSolver(conf: CompetitionConf): Unit = {

    //Parsing the instance and instantiating model declaration
    val parser = XCSP3Parser(conf.benchname())

    val decisionVariables: Array[CPIntVar] = parser.varHashMap.values.toArray

    val solver: CPSolver = parser.cp
    solver.silent = true

    Random.setSeed(conf.randomseed())

    val config = new ALNSConfig(
      timeout = conf.timelimit().toLong * 1000000000L,
      coupled = true,
      learning = true,
      Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
      Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.BinSplit, ALNSBuilder.ConfOrderValLearn, ALNSBuilder.FirstFailValLearn, ALNSBuilder.LastConfValLearn, ALNSBuilder.BinSplitValLearn),
      ALNSBuilder.Priority,
      ALNSBuilder.Priority,
      ALNSBuilder.TTI,
      ALNSBuilder.TTI
    )

    val alns = ALNSSearch(solver, decisionVariables, config)

    val result = alns.search()
    val sols = result.solutions

    if(sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation)
    else CompetitionOutput.printStatus("UNSATISFIABLE")

  }

}
