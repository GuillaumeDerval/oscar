package oscar.xcsp3.competition.solvers

import oscar.algo.search.{Branching, DFSearch}
import oscar.cp.{CPSolver, NoSolutionException, binaryLastConflict, learnValueHeuristic}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.operators.SearchFunctions._
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

/**
  * This Hybrid solver uses a mix of complete search and ALNS search.
  */
object HybridSolver extends CompetitionApp with App {

  override def runSolver(conf: CompetitionConf): Unit = {
    val startTime = System.nanoTime()

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
    }

    if (parsingResult.isDefined) {
      val (vars, solver, solutionGenerator) = parsingResult.get
      solver.silent = true

      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      var optimumFound = false

      val timeout = ((conf.timelimit().toLong - 5L) * 1000000000L) - (System.nanoTime() - startTime)
      var endTime: Long = System.nanoTime() + (if(maximizeObjective.isDefined) (timeout * 0.1).toLong else timeout)

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution{
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        optimumFound = if (maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        if(sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))){
          if(maximizeObjective.isDefined) printObjective(sol.objective)
          sols += ((sol, instantiation))
        }
      }

      val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime || optimumFound

      printComment("Parsing done, starting first complete search")

      var stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
        solver.search(binaryLastConflict(
          vars,
          i => vars(i).size,
          learnValueHeuristic(vars, if(maximizeObjective.isDefined) if(maximizeObjective.get) vars(_).min else vars(_).max else vars(_).max)
        ))
      }

      if(!optimumFound && !stats.completed) {
        printComment("Complete search done, starting ALNS search")

        val config = new ALNSConfig(
          (timeout * 0.7).toLong,
          conf.memlimit(),
          coupled = true,
          learning = true,
          Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
          Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.ExtOriented),
          ALNSBuilder.ValHeurisBoth,
          valLearn = true,
          ALNSBuilder.Priority,
          ALNSBuilder.Priority,
          ALNSBuilder.AvgImprov,
          ALNSBuilder.AvgImprov
        )

        val alns = ALNSSearch(solver, vars, config)
        val result = if(sols.isEmpty) alns.search() else alns.searchFrom(sols.last._1)
        optimumFound = result.optimumFound

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
            else if(bestOperator.contains(ALNSBuilder.ExtOriented)) extentionnalOriented(vars, valMax, valLearn)
            else conflictOrdering(vars, valMax, valLearn)
          }
          else //Default search: Conflict ordering:
            conflictOrdering(vars, valMax = false, valLearn = false)
        }

        endTime = startTime + timeout
        stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
          solver.search(search)
        }
      }

      if (sols.nonEmpty) printSolution(sols.last._2, maximizeObjective.isDefined && (optimumFound || stats.completed))
      else if (stats.completed) printStatus("UNSATISFIABLE")
      else {
        printStatus("UNKNOWN")
        printDiagnostic("NO_SOL_FOUND")
      }
    }
  }
}
