package oscar.xcsp3.competition.solvers

import oscar.algo.search.DFSearch
import oscar.cp.{CPSolver, _}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.xcsp3.XCSP3Parser
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.collection.mutable
import scala.util.Random

object ConfOrderSolver extends CompetitionApp with App{

  override def runSolver(conf: CompetitionConf): Unit = {

    //Parsing the instance
    val parser = XCSP3Parser(conf.benchname())

    val vars: Array[CPIntVar] = parser.varHashMap.values.toArray

    val solver: CPSolver = parser.cp
    solver.silent = true

    Random.setSeed(conf.randomseed())
    val timeout = (conf.timelimit().toLong - 5L) * 1000000000L
    val startTime = System.nanoTime()
    val endTime: Long = startTime + timeout

    val sols = mutable.ArrayBuffer[CPIntSol]()

    solver.onSolution{
      val time = System.nanoTime() - startTime
      val sol = new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time, CPIntSol.getXCSPInstantiation(vars))
      println("o " + sol.objective)
      sols += sol
    }

    val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(conflictOrderingSearch(vars,i => vars(i).size, learnValueHeuristic(vars, i => vars(i).max)))
    }

    if(sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation)
    else CompetitionOutput.printStatus("UNSATISFIABLE")

  }

}
