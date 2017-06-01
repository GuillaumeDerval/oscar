package oscar.xcsp3.competition.solvers

import oscar.algo.search.{Branching, DFSearch}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.operators.SearchFunctions._
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.cp.{CPSolver, NoSolutionException}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object HybridSolver2 extends CompetitionApp with App {

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
    } catch {
      case _: NotImplementedError =>
        printStatus("UNSUPPORTED")
        None

      case _: NoSolutionException =>
        printStatus("UNSATISFIABLE")
        None

      case e => throw e
    }

    if (parsingResult.isDefined) {
      val (vars, solver, solutionGenerator) = parsingResult.get
      solver.silent = true

      val timeout = (conf.timelimit().toLong - 5L) * 1000000000L

      val startTime = System.nanoTime()
      val endTime: Long = startTime + timeout

      val config = new ALNSConfig(
        (timeout * 0.75).toLong,
        conf.memlimit(),
        coupled = true,
        learning = true,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
        Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail),
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

      val sols = mutable.ArrayBuffer[CPIntSol]()

      printComment("Parsing done, starting ALNS search")
      val result = alns.search()
      sols ++= result.solutions

      printComment("ALNS done, starting complete search")

      //Selecting search function based on operator that induced the most improvement:
      val search: Branching = {
        val (bestOperator, opStats) = result.searchStats.maxBy(_._2.improvement)
        if (opStats.improvement > 0) {
          printComment("Best operator: " + bestOperator + " with improvement of: " + opStats.improvement)

          val valLearn = bestOperator.contains("valLearn")
          val valMax = bestOperator.contains("Max")

          if (bestOperator.contains(ALNSBuilder.BinSplit)) binarySplit(vars, valMax, valLearn)
          else if (bestOperator.contains(ALNSBuilder.FirstFail)) firstFail(vars, valMax, valLearn)
          else if (bestOperator.contains(ALNSBuilder.LastConf)) lastConflict(vars, valMax, valLearn)
          else conflictOrdering(vars, valMax, valLearn)
        }
        else //Default search: Conflict ordering with value heuristic:
          conflictOrdering(vars, valMax = false, valLearn = false)
      }

      solver.onSolution {
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time, solutionGenerator())
        printObjective(sol.objective)
        sols += sol
      }

      val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime

      val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
        solver.search(search)
      }

      if (sols.nonEmpty) printSolution(sols.last.instantiation, solver.objective.isOptimum() || stats.completed)
      else if (stats.completed) printStatus("UNSATISFIABLE")
      else {
        printStatus("UNKNOWN")
        printDiagnostic("NO_SOL_FOUND")
      }
    }
  }
}
