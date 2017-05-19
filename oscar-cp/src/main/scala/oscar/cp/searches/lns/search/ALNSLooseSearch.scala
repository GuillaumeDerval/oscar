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

  //Instantiating search operators:
  lazy val searchOps: Array[ALNSOperator] = builder.instantiateSearchOperators

  lazy val relaxStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(relaxOps)
  lazy val searchStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(searchOps)

  lazy val nOpCombinations: Int = relaxOps.filter(_.isActive).map(_.nParamVals).sum * searchOps.filter(_.isActive).map(_.nParamVals).sum

  val removedRelaxations: mutable.HashSet[ALNSOperator] = mutable.HashSet[ALNSOperator]()
  val removedSearches: mutable.HashSet[ALNSOperator] = mutable.HashSet[ALNSOperator]()

  //TODO: implement exponential timeout
  //TODO: add multiobjective support
  override def alnsLearning(): Unit = {
    learning = true
    val initSol = currentSol

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
      val improvement = Math.abs(currentSol.objective - initSol.objective)

      if(!relaxPerf.contains(relax.name)) relaxPerf += relax.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      relaxPerf(relax.name)._2 += ((time, improvement))
      if(!searchPerf.contains(search.name)) searchPerf += search.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      searchPerf(search.name)._2 += ((time, improvement))

      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = initSol
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

    relaxOps.filter(!_.isActive).foreach(operator => {
      relaxStore.remove(operator)
      removedRelaxations += operator
    })
    searchOps.filter(!_.isActive).foreach(operator => {
      searchStore.remove(operator)
      removedSearches += operator
    })

    if(!solver.silent) {
      println(relaxStore.nElements + " relax operators remaining.")
      println(searchStore.nElements + " search operators remaining.")
    }

    if(relaxStore.nElements == 0 || searchStore.nElements == 0) learnRatio *= 2
    else searchFail = 0

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.objective
    endSearch = endTime
    learning = false
  }

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0
    while (System.nanoTime() < endTime  && relaxStore.nonEmpty && searchStore.nonEmpty && (!config.learning || stagnation < stagnationThreshold)) {
      lnsSearch(relaxStore.select(), searchStore.select())
    }
  }

  def lnsSearch(relax: ALNSOperator, search: ALNSOperator): Unit = {
    if(!learning) endSearch = Math.min(System.nanoTime() + iterTimeout * 10, endTime)

    if(!solver.silent) println("Starting new search with: " + relax.name + " and " + search.name)
    if(!solver.silent) println("Operator timeout: " + Math.min(iterTimeout * 10, endTime - System.nanoTime())/1000000000.0 + "s")

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        relax(currentSol)
      }
      catch {
        case _: Inconsistency => relaxDone = false
      }
      if (relaxDone) search(currentSol)
    }

    val improvement = math.abs(currentSol.objective - oldObjective)
    if(improvement > 0){
      stagnation = 0
      if(iterTimeout >= config.timeout || stats.time * 1000000 > iterTimeout) iterTimeout = stats.time * 1000000
    }
    else stagnation += 1

    if (relaxDone) {
      if(!solver.silent) println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      relax.update(improvement, stats, fail = stats.time > iterTimeout * 10)
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (relax.isActive) relaxStore.adapt(relax, metric(relax, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning){
            relaxStore.remove(relax)
            removedRelaxations += relax
          }
        }
      }
      search.update(improvement, stats, fail = stats.time > iterTimeout * 10)
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (search.isActive) searchStore.adapt(search, metric(search, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + search.name + " deactivated")
          if (!learning){
            searchStore.remove(search)
            removedSearches += search
          }
        }
      }
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      relax.update(improvement, stats, fail = true)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) {
        if (relax.isActive) relaxStore.adapt(relax, metric(relax, improvement, stats))
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning){
            relaxStore.remove(relax)
            removedRelaxations += relax
          }
        }
      }
    }
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    relaxOps.map(x => x.name -> x.getStats).toMap,
    searchOps.map(x => x.name -> x.getStats).toMap
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

    if(removedRelaxations.nonEmpty){
      removedRelaxations.foreach(operator => relaxStore.add(operator, metric(operator, 0, new SearchStatistics(0, 0, 0L, false,0L, 0, 0))))
      removedRelaxations.clear()
    }

    if(removedSearches.nonEmpty){
      removedSearches.foreach(operator => searchStore.add(operator, metric(operator, 0, new SearchStatistics(0, 0, 0L, false,0L, 0, 0))))
      removedSearches.clear()
    }
  }
}
