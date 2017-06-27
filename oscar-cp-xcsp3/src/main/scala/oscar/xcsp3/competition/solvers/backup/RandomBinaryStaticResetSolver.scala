package oscar.xcsp3.competition.solvers.backup

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
import scala.util.Random

object RandomBinaryStaticResetSolver extends CompetitionApp with App {

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

    if (parsingResult.isDefined) {
      val (vars, solver, solutionGenerator) = parsingResult.get
      solver.silent = true

      val timeout = ((conf.timelimit().toLong - 5L) * 1000000000L) - (System.nanoTime() - startTime)
      val endTime: Long = System.nanoTime() + timeout

      val isCOP: Boolean = solver.objective.objs.nonEmpty
      var optimumFound = false

      val sols = mutable.ArrayBuffer[(CPIntSol, String)]()
      solver.onSolution {
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time)
        val instantiation = solutionGenerator()
        optimumFound = if(isCOP) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        updateSol(instantiation, sol.objective, isCOP)
        sols += ((sol, instantiation))
      }

      printComment("Parsing done, starting search")

      var stats: SearchStatistics = null
      //reset every 0.5 sec, binary static on the vars randomized
      while (System.nanoTime() <= endTime) {
        val var2 = Random.shuffle(vars.toSeq).toArray
        val maxTime = Math.min(500000 + System.nanoTime(), endTime)
        val stopCondition = (s: DFSearch) => {
          var stop = false
          stop |= (System.nanoTime() >= maxTime)
          stop |= optimumFound
          stop
        }
        stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
          solver.search(
            binaryStatic(var2)
          )
        }
      }
      if (sols.nonEmpty){
        if(isCOP && (optimumFound || stats.completed)) status = "OPTIMUM FOUND"
      }
      else if (stats.completed) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }
}
