package oscar.xcsp3.competition.solvers

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf, CompetitionOutput}

import scala.util.Random

object ALNSSolver2 extends CompetitionApp with App{

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

    val config = new ALNSConfig(
      timeout = timeout,
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

    val result = alns.search()
    val sols = result.solutions

    if(sols.nonEmpty) CompetitionOutput.printSolution(sols.last.instantiation)
    else CompetitionOutput.printStatus("UNSATISFIABLE")

  }

}
