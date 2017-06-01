package oscar.xcsp3.competition.solvers

import oscar.cp.{CPSolver, NoSolutionException}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.xcsp3.XCSP3Parser
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

object ALNSSolver extends CompetitionApp with App{

  override def runSolver(conf: CompetitionConf): Unit = {

    //Parsing the instance:
    val parser = try {
      Some(XCSP3Parser(conf.benchname()))
    } catch{
      case _: NotImplementedError =>
        printStatus("UNSUPPORTED")
        None

      case _: NoSolutionException =>
        printStatus("UNSATISFIABLE")
        None

      case e => throw e
    }

    if(parser.isDefined){
      val vars: Array[CPIntVar] = parser.get.varHashMap.values.toArray

      val solver: CPSolver = parser.get.cp

      solver.silent = true
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
        () => CPIntSol.getXCSPInstantiation(vars),
        () => printObjective(solver.objective.objs.head.best)
      )

      val alns = ALNSSearch(solver, vars, config)

      printComment("Parsing done, starting search")

      val result = alns.search()
      val sols = result.solutions

      if(sols.nonEmpty) printSolution(sols.last.instantiation, solver.objective.isOptimum())
      else{
        printStatus("UNKNOWN")
        printDiagnostic("NO_SOL_FOUND")
      }
    }
  }
}
