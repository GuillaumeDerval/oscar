package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.algo.search.SearchStatistics
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator, ALNSSingleParamOperator, ALNSTwoParamsOperator}
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable
import scala.util.Random

class ALNSLooseSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  //Instantiating relax operators:
  lazy val relaxOps: Array[ALNSOperator] = builder.instantiateRelaxOperators
  lazy val relaxStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(relaxOps)

  //Instantiating search operators:
  lazy val searchOps: Array[ALNSOperator] = builder.instantiateSearchOperators
  lazy val searchStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(searchOps)

  lazy val nOpCombinations: Int = relaxOps.filter(_.isActive).map(_.nParamVals).sum * searchOps.filter(_.isActive).map(_.nParamVals).sum

  override def alnsLearning(): Unit = {
    learning = true
    val initSol = currentSol.get

    val learningStart = System.nanoTime()
    val tAvail = (((endTime - learningStart) * learnRatio) / nOpCombinations).toLong
    val relaxPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()
    val searchPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()

    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    }{
      val start = System.nanoTime()
      endSearch = Math.min(start + tAvail, endTime)

      lnsSearch(relax, search)

      val time = System.nanoTime() - start
      val improvement = Math.abs(currentSol.get.objective - initSol.objective)

      if(!relaxPerf.contains(relax.name)) relaxPerf += relax.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      relaxPerf(relax.name)._2 += ((time, improvement))
      if(!searchPerf.contains(search.name)) searchPerf += search.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      searchPerf(search.name)._2 += ((time, improvement))

      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = Some(initSol)
    }

    relaxPerf.values.filter{ case (op, perfs) =>
      op.isActive && perfs.map(_._2).max == 0
    }.foreach{ case (op, _) =>
      op.setActive(false)
      if(!solver.silent) println("Operator " + op.name + " deactivated")
    }

    searchPerf.values.filter{ case (op, perfs) =>
      op.isActive && perfs.map(_._2).max == 0
    }.foreach{ case (op, _) =>
      op.setActive(false)
      if(!solver.silent) println("Operator " + op.name + " deactivated")
    }

    relaxOps.filter(!_.isActive).foreach(relaxStore.deactivate)
    searchOps.filter(!_.isActive).foreach(searchStore.deactivate)

    if(!solver.silent) {
      println(relaxStore.nActive + " relax operators remaining.")
      println(searchStore.nActive + " search operators remaining.")
    }

    if(relaxStore.nActive == 0 || searchStore.nActive == 0) learnRatio *= 2
    else searchFail = 0

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.get.objective
    endSearch = endTime
    learning = false
  }

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0
    while (System.nanoTime() < endTime  && relaxStore.nonActiveEmpty && searchStore.nonActiveEmpty && (!config.learning || stagnation < stagnationThreshold) && !optimumFound) {
      lnsSearch(relaxStore.select(), searchStore.select())
    }
  }

  def lnsSearch(relax: ALNSOperator, search: ALNSOperator): Unit = {
    if(!learning) endSearch = Math.min(System.nanoTime() + iterTimeout * 10, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endSearch - System.nanoTime())/1000000000.0 + "s")
    }

    val oldObjective = currentSol.get.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        relax(currentSol.get)
      }
      catch {
        case _: Inconsistency => relaxDone = false
      }
      if (relaxDone) search(currentSol.get)
    }

    val improvement = math.abs(currentSol.get.objective - oldObjective)

    if(improvement > 0){
      stagnation = 0
      if(iterTimeout >= config.timeout || stats.time * 1000000 > iterTimeout) iterTimeout = stats.time * 1000000
    }
    else stagnation += 1

    if (relaxDone) {
      if(stats.completed){
        if(!solver.silent) println("Search space completely explored, improvement: " + improvement)
        //Updating probability distributions:
        relax.update(improvement, stats, fail = !learning)
        search.update(improvement, stats, fail = false)
        relax.setActive(false)
      }
      else {
        if(!solver.silent) println("Search done, Improvement: " + improvement)
        //Updating probability distributions:
        relax.update(improvement, stats, fail = !learning && stats.time > iterTimeout * 10)
        search.update(improvement, stats, fail = !learning && stats.time > iterTimeout * 10)
      }

      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (relax.isActive) relaxStore.adapt(relax, metric(relax, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) relaxStore.deactivate(relax)
        }
      }
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (search.isActive) searchStore.adapt(search, metric(search, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + search.name + " deactivated")
          if (!learning) searchStore.deactivate(search)
        }
      }
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement)
      //Updating only relax as the the search has not been done:
      relax.update(improvement, stats, fail = !learning)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) {
        if (relax.isActive) relaxStore.adapt(relax, metric(relax, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) relaxStore.deactivate(relax)
        }
      }
    }
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    relaxOps.map(x => x.name -> x.getStats).toMap,
    searchOps.map(x => x.name -> x.getStats).toMap,
    maximizeObjective.isDefined & optimumFound,
    solsFound.isEmpty & optimumFound
  )

  private def getReifiedOperators(operators: Seq[ALNSOperator]) = operators.flatMap{
    case op: ALNSSingleParamOperator[_] => op.getReifiedParameters
    case op: ALNSTwoParamsOperator[_,_] => op.getReifiedParameters
    case op: ALNSOperator => Seq(op)
  }

  /**
    * resets the store(s)
    */
  override def resetStore(): Unit = {
    relaxOps.foreach(operator => {
      operator.resetFails()
      operator.setActive(true)
    })
    searchOps.foreach(operator => {
      operator.resetFails()
      operator.setActive(true)
    })
    relaxStore.reset()
    searchStore.reset()
  }
}
