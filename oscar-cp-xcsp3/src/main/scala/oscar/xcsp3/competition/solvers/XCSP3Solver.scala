package oscar.xcsp3.competition.solvers

import oscar.algo.Inconsistency
import oscar.cp.heuristics.{ActivityBasedSearch, BoundImpactValueSelector, Hybridization, InitHeuristics, LeastConstrainingValueSelector, ObjectiveBasedSelector, PhaseSaving, SolBasedPhaseSaving, WeightedDegree}
import oscar.cp.nogoods.database.NogoodDB
import oscar.cp.nogoods.searches.{ConflictOrderingSearch, HeuristicNogoodBranching, NogoodSearch}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.{CPSolver, _}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object XCSP3Solver extends CompetitionApp with App {


  override def runSolver(conf: CompetitionConf): Unit = {
    val startTime = System.nanoTime()

    val md = new ModelDeclaration

    //Parsing the instance
    printComment("Parsing instance...")
    val parsingResult = try {
      val (decisionVars, auxiliaryVars, solutionGenerator) = XCSP3Parser2.parse2(md, conf.benchname())

      val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
      md.setCurrentModel(model)

      val cpDecisionVars: Array[CPIntVar] = decisionVars.map(model.getRepresentative(_).realCPVar)
      val cpAuxiliaryVars: Array[CPIntVar] = auxiliaryVars.map(model.getRepresentative(_).realCPVar)
      val solver: CPSolver = model.cpSolver

      Some(cpDecisionVars, cpAuxiliaryVars, solver, solutionGenerator)
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
      val (decisionVars, auxiliaryVars, solver, solutionGenerator) = parsingResult.get
      val vars = decisionVars ++ auxiliaryVars
      solver.silent = true
      val timeout = ((conf.timelimit() -5).toLong * 1000000000L) - (System.nanoTime() - tstart)
      val endTime = System.nanoTime() + timeout
      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      val isMin = solver.objective.objs.head.isMin
      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      val alpha = if(isMin) -1 else 0
      val beta = if(isMin) 0 else -1
      var optimumFound = false
      var lastSolTime = 0L

      println("old method -> number of decision vars: " + decisionVars.length)

      val bivs = new BoundImpactValueSelector(solver, vars)
      val init = new InitHeuristics(solver, vars, bivs.selectValue, minRounds = 15, maxTime = 10000000000L, significance = 0.25)
      val isDecisionVar = bivs.isDecisionVar
      val (abs, obs, wdeg) = init.getFeatureArrays
      val decisionIndices = isDecisionVar.zipWithIndex.filter(x => x._1).map(i => i._2)


      println("new method (bivs) -> number of decision vars: " + decisionIndices.length)

      // decision variables according to observed objective bound modifications
      val dVars = vars.zipWithIndex.filter(i => decisionIndices.contains(i._2)).map(i => i._1)
      val aVars = vars.zipWithIndex.filterNot(i => decisionIndices.contains(i._2)).map(i => i._1)

      val dActivity = abs.zipWithIndex.filter(i => decisionIndices.contains(i._2)).map(i => i._1)
      val aActivity = abs.zipWithIndex.filterNot(i => decisionIndices.contains(i._2)).map(i => i._1)
      val dDeltaO = obs.zipWithIndex.filter(i => decisionIndices.contains(i._2)).map(i => i._1)
      val dDegree = wdeg.zipWithIndex.filter(i => decisionIndices.contains(i._2)).map(i => i._1)
      val aDegree = wdeg.zipWithIndex.filterNot(i => decisionIndices.contains(i._2)).map(i => i._1)

      val dABS = new ActivityBasedSearch(dVars, 0.999)
      dABS.setActivity(dActivity)
      val dOBS = new ObjectiveBasedSelector(solver, dVars, alpha, beta, 0.99)
      dOBS.setDeltaO(dDeltaO)
      val dWDEG = new WeightedDegree(solver, dVars, 0.999)
      dWDEG.setDegree(dDegree)

      val  luby = Array(3000000000L, 3000000000L, 3000000000L, 3000000000L, 4000000000L,
        4000000000L, 4000000000L, 5000000000L, 5000000000L,
        5000000000L, 7000000000L, 8000000000L)



      var current:Long = 0L
      var searchTime:Long = (timeout * 0.05).toLong
      var limit:Long = 0L
      val geoRate = 1.3

      var stopCondition = (_: NogoodSearch) => {
        val now = System.nanoTime()
        var stop = false
        stop |= now >= limit
        stop |= now >= endTime
        stop |= sols.nonEmpty
        stop
      }

      val noGoodDB = NogoodDB()
      val noGoodSearch = new NogoodSearch(solver, noGoodDB)


      // Initialize solution based phase saving for later
      val sbps:(PhaseSaving, PhaseSaving) = if(!aVars.isEmpty) {

        val sbpsDec = new SolBasedPhaseSaving(dVars, new BoundImpactValueSelector(solver, dVars).selectValue)
        val sbpsAux = new SolBasedPhaseSaving(aVars, new LeastConstrainingValueSelector(aVars).selectValue)

        noGoodSearch.onSolution {
          val time = System.nanoTime() - startTime
          lastSolTime = time
          val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
          sbpsDec.updateSolution()
          sbpsAux.updateSolution()
          val instantiation = solutionGenerator()
          optimumFound = if (maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
          if (sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))) {
            updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
            sols += ((sol, instantiation))
          }
        }
        (sbpsDec, sbpsAux)
      }
      else {

        val sbpsDec = new SolBasedPhaseSaving(dVars, new BoundImpactValueSelector(solver, dVars).selectValue)

        noGoodSearch.onSolution {
          val time = System.nanoTime() - startTime
          lastSolTime = time
          val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
          sbpsDec.updateSolution()
          val instantiation = solutionGenerator()
          optimumFound = if (maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
          if (sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))) {
            updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
            sols += ((sol, instantiation))
          }
        }
        (sbpsDec, null)
      }


      current = System.nanoTime()
      limit = current + searchTime

      val c = new Hybridization(0.5, dOBS.getScaledDeltaO, dABS.getScaledActivity)


      var searchStrat = if(aVars.isEmpty) {
        val h = new HeuristicNogoodBranching(dVars, c.hybridize, new BoundImpactValueSelector(solver, dVars).selectValue)
        ConflictOrderingSearch(h)(solver)
      }
      else {
        val aABS = new ActivityBasedSearch(aVars, 0.999)
        aABS.setActivity(aActivity)
        val aWDEG = new WeightedDegree(solver, aVars, 0.999)
        aWDEG.setDegree(aDegree)
        val aC = new Hybridization(0.5, aABS.getScaledActivity, aWDEG.getScaledDegree)
        val h1 = new HeuristicNogoodBranching(dVars, c.hybridize, learnValueHeuristic(dVars, new BoundImpactValueSelector(solver, dVars).selectValue))

        //val h2 = new HeuristicNogoodBranching(aVars, aC.hybridize,  new LeastConstrainingValueSelector(aVars).selectValue)
        val h2 = new HeuristicNogoodBranching(aVars, aC.hybridize,  i => aVars(i).getMin)
        ConflictOrderingSearch(h1)(solver) ++ ConflictOrderingSearch(h2)(solver)
      }

      while(sols.isEmpty && current < endTime) {
        println("reset phase 1 ")
        noGoodSearch.start(searchStrat, stopCondition)
        solver.add(noGoodDB.allNogoods().map(ng => ng.toConstraint))
        noGoodDB.clear()
        current = System.nanoTime()
        searchTime = math.ceil(searchTime * geoRate).toLong
        limit = current + searchTime
      }
      printStatus()
    }

  }
}
