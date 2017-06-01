package oscar.xcsp3.competition.solvers

import oscar.cp.{CPSolver, NoSolutionException}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

object ALNSSolver2 extends CompetitionApp with App {

  override def runSolver(conf: CompetitionConf): Unit = {

    val md = new ModelDeclaration

    //Parsing the instance
    val parsingResult = try {
      val (vars, solutionGenerator) = XCSP3Parser2.parse(md, conf.benchname())

      val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
      md.setCurrentModel(model)

      val cpVars: Array[CPIntVar] = vars.map(model.getRepresentative(_).realCPVar)
      val solver: CPSolver = model.cpSolver

      Some(cpVars, solver, solutionGenerator)
    }catch {
      case _: NotImplementedError =>
        printStatus("UNSUPPORTED")
        None

      case _: NoSolutionException =>
        printStatus("UNSATISFIABLE")
        None

      case e => throw e
    }

    if (parsingResult.isDefined){
      val (vars, solver, solutionGenerator) = parsingResult.get
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
        solutionGenerator,
        () => printObjective(solver.objective.objs.head.best)
      )

      val alns = ALNSSearch(solver, vars, config)

      printComment("Parsing done, starting search")

      val result = alns.search()
      val sols = result.solutions

      if (sols.nonEmpty) printSolution(sols.last.instantiation, solver.objective.isOptimum())
      else {
        printStatus("UNKNOWN")
        printDiagnostic("NO_SOL_FOUND")
      }
    }
  }
}
