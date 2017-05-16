package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.algo.search.SearchStatistics
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ALNSCoupledSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  //Instantiating operators:
  lazy val operators: Array[ALNSOperator] = builder.instantiateCoupledOperators

  lazy val opStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(operators)

  val removedOperators: mutable.HashSet[ALNSOperator] = mutable.HashSet[ALNSOperator]()

  //TODO: implement exponential timeout
  //TODO: add multiobjective support
  override def alnsLearning(): Unit = {
    learning = true
    val initSol = currentSol

    val learningStart = System.nanoTime()
    val tAvail = (((endTime - learningStart) * learnRatio) / operators.length).toLong
    val opPerf = ArrayBuffer[(ALNSOperator, Long, Int)]()

    Random.shuffle(operators.toSeq).foreach(operator =>{
      val start = System.nanoTime()
      endSearch = start + tAvail

      lnsSearch(operator)

      val time = System.nanoTime() - start
      val improvement = Math.abs(currentSol.objective - initSol.objective)
      opPerf += ((operator, time, improvement))

      //Restoring initial objective:
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = initSol
    })

    opPerf.filter{case (op, time, improvement) =>
      op.isActive && improvement == 0
    }.foreach{case (op, _, _) =>
      op.setActive(false)
      if(!solver.silent) println("Operator " + op.name + " deactivated.")
      opStore.remove(op)
      removedOperators += op
    }

    if(!solver.silent) println(opStore.nElements + " operators remaining.")

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.objective
    endSearch = endTime
    learning = false
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime && opStore.nonEmpty && stagnation < stagnationThreshold) {
      stagnation = 0
      lnsSearch(opStore.select())
    }
  }

  def lnsSearch(operator: ALNSOperator): Unit = {
    if(!learning) endSearch = Math.min(System.nanoTime() + maxSuccessOpTime * 10, endTime)

    if(!solver.silent) println("Starting new search with: " + operator.name)
    if(!solver.silent) println("Operator timeout: " + Math.min(maxSuccessOpTime * 10, endTime - System.nanoTime())/1000000000.0 + "s")

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        operator(currentSol)
      }
      catch {
        case i: Inconsistency => relaxDone = false
      }
    }

    val improvement = math.abs(currentSol.objective - oldObjective)
    if(improvement > 0){
      stagnation = 0
      if(maxSuccessOpTime >= config.timeout || stats.time * 1000000 > maxSuccessOpTime) maxSuccessOpTime = stats.time * 1000000
    }
    else stagnation += 1

    if (relaxDone) {
      if(!solver.silent) println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      operator.update(improvement, stats, fail = stats.time > maxSuccessOpTime * 10)
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      operator.update(improvement, stats, fail = true)
    }

    if(!operator.isInstanceOf[ALNSReifiedOperator]) {
      if (operator.isActive)
        opStore.adapt(operator, metric(operator, improvement, stats))
      else {
        if (!solver.silent) println("Operator " + operator.name + " deactivated.")
        opStore.remove(operator)
        removedOperators += operator
      }
    }
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators.map(x => x.name -> x.getStats).toMap
  )

  /**
    * resets the store(s)
    */
  override def resetStore(): Unit = {
    operators.foreach(_.setActive(true))

    if(removedOperators.nonEmpty){
      removedOperators.foreach(operator => opStore.add(operator, metric(operator, 0, new SearchStatistics(0, 0, 0L, false,0L, 0, 0))))
      removedOperators.clear()
    }
  }
}
