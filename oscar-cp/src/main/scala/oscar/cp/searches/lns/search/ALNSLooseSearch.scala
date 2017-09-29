package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators._
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable
import scala.util.Random

class ALNSLooseSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  //Instantiating relax operators:
  lazy val relaxStore: AdaptiveStore[ALNSOperator] = config.relaxStore.get
  lazy val relaxOps: Array[ALNSOperator] = relaxStore.getElements.toArray

  //Instantiating search operators:
  lazy val searchStore: AdaptiveStore[ALNSOperator] = config.searchStore
  lazy val searchOps: Array[ALNSOperator] = searchStore.getElements.toArray

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
      endIter = Math.min(start + tAvail, endTime)

      lnsIter(relax, search)

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

    if(config.opDeactivation) {
      relaxPerf.values.filter { case (op, perfs) =>
        op.isActive && perfs.map(_._2).max == 0
      }.foreach { case (op, _) =>
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated")
      }

      searchPerf.values.filter { case (op, perfs) =>
        op.isActive && perfs.map(_._2).max == 0
      }.foreach { case (op, _) =>
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated")
      }

      relaxOps.filter(!_.isActive).foreach(relaxStore.deactivate)
      searchOps.filter(!_.isActive).foreach(searchStore.deactivate)
    }

    if(!solver.silent) {
      println(relaxStore.nActive + " relax operators remaining.")
      println(searchStore.nActive + " search operators remaining.")
    }

    if(relaxStore.nActive == 0 || searchStore.nActive == 0) learnRatio *= 2
    else searchFail = 0

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.get.objective
    endIter = endTime
    learning = false
  }

  override def onStagnation(): Unit = ???

  override def spotTuning(): Unit = ???

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0
    while (System.nanoTime() < endTime  && relaxStore.nonActiveEmpty && searchStore.nonActiveEmpty && (!config.learning || stagnation < stagnationThreshold) && !optimumFound) {
      lnsIter(relaxStore.select(), searchStore.select())
    }
  }

  def lnsIter(relax: ALNSOperator, search: ALNSOperator): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (relaxFunction, _, _) = relax.getFunction
    val (searchFunction, searchFailures, searchDiscrepancy) = search.getFunction
    if(searchFailures.isDefined) nFailures = searchFailures.get

    var relaxDone = true
    val oldObjective = currentSol.get.objective
    val iterStart = System.nanoTime()

    val stats = solver.startSubjectTo(stopCondition, searchDiscrepancy.getOrElse(Int.MaxValue), null) {
      try {
        relaxFunction(currentSol.get)
      }
      catch {
        case _: Inconsistency => relaxDone = false
      }
      if (relaxDone) searchFunction(currentSol.get)
    }

    val iterEnd = System.nanoTime()
    val newObjective = currentSol.get.objective

    if(searchFailures.isDefined) nFailures = 0 //Restauring failures number to 0

    val improvement = math.abs(currentSol.get.objective - oldObjective)
    val time = iterEnd - iterStart

    if(improvement > 0){
      stagnation = 0
      if(iterTimeout >= config.timeout || time > iterTimeout) iterTimeout = time * 2
    }
    else stagnation += 1

    if (relaxDone) {
      //Updating probability distributions:
      relax.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = false, iter)
      search.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = false, iter)
      if(stats.completed)
        if(!solver.silent) println("Search space completely explored, improvement: " + improvement)
      else
        if(!solver.silent) println("Search done, Improvement: " + improvement)

      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (relax.isActive) relaxStore.adapt(relax)
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) relaxStore.deactivate(relax)
        }
      }
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (search.isActive) searchStore.adapt(search)
        else {
          if (!solver.silent) println("Operator " + search.name + " deactivated")
          if (!learning) searchStore.deactivate(search)
        }
      }
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement)
      //Updating only relax as the the search has not been done:
      relax.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = !learning, iter)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) {
        if (relax.isActive) relaxStore.adapt(relax)
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) relaxStore.deactivate(relax)
        }
      }
    }

    iter += 1
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    relaxOps,
    searchOps,
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
