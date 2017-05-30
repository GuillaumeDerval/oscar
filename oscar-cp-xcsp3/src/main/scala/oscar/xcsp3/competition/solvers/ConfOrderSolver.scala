package oscar.xcsp3.competition.solvers

import oscar.algo.search.DFSearch
import oscar.cp.{CPSolver, _}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.collection.mutable
import scala.util.Random

object ConfOrderSolver extends CompetitionApp with App{

  override def runSolver(conf: CompetitionConf): Unit = {

    val md = new ModelDeclaration

    //Parsing the instance and instantiating model declaration
    val (vars, solutionGenerator) = XCSP3Parser2.parse(md, conf.benchname())

    val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
    md.setCurrentModel(model)

    val decisionVariables: Array[CPIntVar] = vars.map(model.getRepresentative(_).realCPVar)

    val solver: CPSolver = model.cpSolver
    solver.silent = true

    Random.setSeed(conf.randomseed())
    val timeout = (conf.timelimit().toLong - 5L) * 1000000000L
    val startTime = System.nanoTime()
    val endTime: Long = startTime + timeout

    val maximizeObjective: Boolean = solver.objective.objs.head.isMax

    val sols = mutable.ArrayBuffer[CPIntSol]()

    solver.onSolution{
      val time = System.nanoTime() - startTime
      val sol = new CPIntSol(decisionVariables.map(_.value), solver.objective.objs.head.best, time, solutionGenerator())
      println("o " + sol.objective)
      sols += sol
    }

    val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(
        conflictOrderingSearch(
          decisionVariables,
          i => decisionVariables(i).size,
          learnValueHeuristic(decisionVariables, if(maximizeObjective) decisionVariables(_).min else decisionVariables(_).max)
        )
      )
    }

    if(sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation, solver.objective.isOptimum() || stats.completed)
    else if(stats.completed) CompetitionOutput.printStatus("UNSATISFIABLE")
    else{
      CompetitionOutput.printStatus("UNKNOWN")
      CompetitionOutput.printDiagnostic("NO_SOL_FOUND")
    }
  }
}
