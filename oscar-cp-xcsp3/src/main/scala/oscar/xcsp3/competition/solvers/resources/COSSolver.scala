package oscar.xcsp3.competition.solvers.resources

import oscar.algo.Inconsistency
import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.{CPSolver, _}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

abstract class COSSolver extends CompetitionApp with App{

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
    }catch {
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

      val timeout = ((conf.timelimit() -5).toLong * 1000000000L) - (System.nanoTime() - tstart)
      val endTime: Long = System.nanoTime() + timeout

      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      var optimumFound = false

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution {
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), if(maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        optimumFound = if(maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
        sols += ((sol, instantiation))
      }

      val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime || optimumFound

      printComment("Parsing done, starting search...")

      val stats = startSearch(solver, stopCondition, vars, maximizeObjective)

      if (sols.nonEmpty){
        if(maximizeObjective.isDefined && (optimumFound || stats.completed)) status = "OPTIMUM FOUND"
      }
      else if (stats.completed) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }

  def startSearch(solver: CPSolver, stopCondition : DFSearch => Boolean, vars: Array[CPIntVar], maximizeObjective: Option[Boolean]) = {
    solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(
        conflictOrderingSearch(
          vars,
          i => vars(i).size,
          learnValueHeuristic(vars, if(maximizeObjective.isDefined) if(maximizeObjective.get) vars(_).min else vars(_).max else vars(_).max)
        )
      )
    }
  }
}

