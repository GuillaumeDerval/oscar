package oscar.xcsp3.competition.solvers.backup

import oscar.algo.search.DFSearch
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.{CPSolver, _}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object ExtentionnalOrientedSolver extends CompetitionApp with App{

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
    }catch {
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

      val timeout = ((conf.timelimit().toLong - 5L) * 1000000000L) - (System.nanoTime() - startTime)
      val endTime: Long = System.nanoTime() + timeout

      val maximizeObjective: Option[Boolean] = if (solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      var optimumFound = false

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution {
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), if(maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        optimumFound = if(maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        if(maximizeObjective.isDefined) printObjective(sol.objective)
        sols += ((sol, instantiation))
      }

      printComment("Parsing done, starting search")

      val stopCondition = (s: DFSearch) => {
        var stop = false
        stop |= (System.nanoTime() >= endTime)
        stop
      }
      val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
        solver.search(
          binaryIdx(vars,i => -(vars(i).constraintDegree << 7) / vars(i).size, i => vars(i).min)
        )
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
