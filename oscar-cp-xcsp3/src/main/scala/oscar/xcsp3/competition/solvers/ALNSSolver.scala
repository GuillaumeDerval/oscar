package oscar.xcsp3.competition.solvers

import oscar.algo.Inconsistency
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.cp.{CPSolver, NoSolutionException}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object ALNSSolver extends CompetitionApp with App {

  override def runSolver(conf: CompetitionConf): Unit = {
    val startTime = System.nanoTime()

    val md = new ModelDeclaration

    //Parsing the instance
    printComment("Parsing instance...")
    val parsingResult = try {
      val (vars, solutionGenerator) = XCSP3Parser2.parse(md, conf.benchname())

      val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
      md.setCurrentModel(model)

      val cpVars: Array[CPIntVar] = vars.map(model.getRepresentative(_).realCPVar)
      val solver: CPSolver = model.cpSolver

      Some(cpVars, solver, solutionGenerator)
    } catch {
      case _: NotImplementedError =>
        status = "UNSUPPORTED"
        printStatus()
        None

      case _: NoSolutionException =>
        status = "UNSATISFIABLE"
        printStatus()
        None

      case _: Inconsistency =>
        status = "UNSATISFIABLE"
        printStatus()
        None
    }

    if (parsingResult.isDefined){
      val (vars, solver, solutionGenerator) = parsingResult.get
      solver.silent = true

      val timeout = ((conf.timelimit().toLong - 5L) * 1000000000L) - (System.nanoTime() - startTime)

      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution{
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        if(sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))){
          updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
          sols += ((sol, instantiation))
        }
      }

      val config = new ALNSConfig(
        timeout,
        conf.memlimit(),
        coupled = true,
        learning = true,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided, ALNSBuilder.FullRelax),
        Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.ExtOriented, ALNSBuilder.WeightDeg),
        ALNSBuilder.ValHeurisBoth,
        valLearn = true,
        ALNSBuilder.Priority,
        ALNSBuilder.Priority,
        ALNSBuilder.AvgImprov,
        ALNSBuilder.AvgImprov
      )

      val alns = ALNSSearch(solver, vars, config)

      printComment("Parsing done, starting search...")

      val result = alns.search()

      if (sols.nonEmpty){
        if(solver.objective.objs.nonEmpty && result.optimumFound) status = "OPTIMUM FOUND"
      }
      else if(result.unsat) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }
}
