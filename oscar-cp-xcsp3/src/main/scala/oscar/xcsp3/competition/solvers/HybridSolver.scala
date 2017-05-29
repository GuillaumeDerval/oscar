package oscar.xcsp3.competition.solvers

import oscar.algo.search.DFSearch
import oscar.cp.{CPSolver, conflictOrderingSearch, learnValueHeuristic}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.collection.mutable
import scala.util.Random

object HybridSolver extends CompetitionApp with App {

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

    val config = new ALNSConfig(
      timeout = (timeout * 0.75).toLong,
      coupled = true,
      learning = true,
      Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided),
      Array(ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.BinSplit, ALNSBuilder.ConfOrderValLearn, ALNSBuilder.FirstFailValLearn, ALNSBuilder.LastConfValLearn, ALNSBuilder.BinSplitValLearn),
      ALNSBuilder.Priority,
      ALNSBuilder.Priority,
      ALNSBuilder.TTI,
      ALNSBuilder.TTI,
      solutionGenerator
    )

    val alns = ALNSSearch(solver, decisionVariables, config)

    val sols = mutable.ArrayBuffer[CPIntSol]()

    val result = alns.search()
    sols ++= result.solutions

    CompetitionOutput.printComment("ALNS done, starting conflict ordering search (" + (endTime - System.nanoTime())/1000000000L + "s remaining)")

    solver.onSolution {
      val time = System.nanoTime() - startTime
      val sol = new CPIntSol(decisionVariables.map(_.value), solver.objective.objs.head.best, time, /*solutionGenerator()*/CPIntSol.getXCSPInstantiation(decisionVariables)) //TODO: fix this
      println("o " + sol.objective)
      sols += sol
    }

    val stopCondition = (_: DFSearch) => System.nanoTime() >= endTime

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      solver.search(conflictOrderingSearch(decisionVariables, i => decisionVariables(i).size, learnValueHeuristic(decisionVariables, i => decisionVariables(i).max)))
    }

    if (sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation)
    else CompetitionOutput.printStatus("UNSATISFIABLE")

  }
}
