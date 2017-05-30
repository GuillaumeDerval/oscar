package oscar.xcsp3.competition.solvers

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.xcsp3.XCSP3Parser
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.util.Random

object ALNSSolver extends CompetitionApp with App{

  override def runSolver(conf: CompetitionConf): Unit = {

    //Parsing the instance
    val parser = XCSP3Parser(conf.benchname())

    val vars: Array[CPIntVar] = parser.varHashMap.values.toArray

    val solver: CPSolver = parser.cp
    solver.silent = true

    Random.setSeed(conf.randomseed())
    val timeout = (conf.timelimit().toLong - 5L) * 1000000000L

    val config = new ALNSConfig(
      timeout,
      conf.memlimit(),
      coupled = true,
      learning = true,
      Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
      Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.BinSplit),
      ALNSBuilder.ValHeurisBoth,
      valLearn = true,
      ALNSBuilder.Priority,
      ALNSBuilder.Priority,
      ALNSBuilder.TTI,
      ALNSBuilder.TTI,
      () => CPIntSol.getXCSPInstantiation(vars)
    )

    val alns = ALNSSearch(solver, vars, config)

    val result = alns.search()
    val sols = result.solutions

    if(sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation, solver.objective.isOptimum())
    else{
      CompetitionOutput.printStatus("UNKNOWN")
      CompetitionOutput.printDiagnostic("NO_SOL_FOUND")
    }

  }

}
