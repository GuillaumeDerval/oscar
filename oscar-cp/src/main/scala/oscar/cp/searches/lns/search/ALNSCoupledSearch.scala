package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ALNSCoupledSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  lazy val opStore: AdaptiveStore[ALNSOperator] = config.searchStore
  lazy val operators: Array[ALNSOperator] = opStore.getElements.toArray

  override def alnsLearning(): Unit = {
    learning = true
    val initSol = currentSol.get

//    solver.onSolution {
//      val time = System.nanoTime() - startTime
//      val objective = solver.objective.objs.head.best
//      println(time/1000000000 + "," + objective)
//    }
//    println("0," + solver.objective.objs.head.best)

    val learningStart = System.nanoTime()
    val tAvail = (((endTime - learningStart) * learnRatio) / operators.length).toLong
//    val tAvail = 600000000000L
    val opPerf = ArrayBuffer[(ALNSOperator, Long, Int)]()

    Random.shuffle(operators.toSeq).foreach(operator =>{
//    operators.foreach(operator =>{
//      println(operator.name)
      val start = System.nanoTime()
      endIter = start + tAvail

      while(System.nanoTime() < endIter) lnsIter(operator)

      val time = System.nanoTime() - start
      val improvement = Math.abs(currentSol.get.objective - initSol.objective)
      opPerf += ((operator, time, improvement))

      //Restoring initial objective:
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = Some(initSol)
    })

    if(!solver.silent){
      println("Operators performances:")
      println("operator, time, objective")
      println(opPerf.map{ case(operator, time, objective) => operator.name + ", " + time/1000000000 + ", " + objective}.mkString("\n"))
    }

    //Stopping search here:
//    optimumFound = true

    if(config.opDeactivation) {
      opPerf.filter { case (op, time, improvement) =>
        op.isActive && improvement == 0
      }.foreach { case (op, _, _) =>
        op.setActive(false)
        opStore.deactivate(op)
        if (!solver.silent) println("Operator " + op.name + " deactivated.")
      }
    }

    if(!solver.silent) println(opStore.nActive + " operators remaining.")

    if(opStore.nActive == 0) learnRatio *= 2
    else searchFail = 0

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.get.objective
    endIter = endTime
    learning = false
  }

  override def onStagnation(): Unit = {
    if(!solver.silent) println("Stagnation")

    learning = true
    val initSol = previousBest.get
    val bestObjective = bestSol.get.objective
    val tAvail = iterTimeout
    currentSol = Some(initSol)

    val sortedOps = operators.toSeq.sortBy(op => iter - op.lastSuccessIter)
    var i = 0
    var newSolFound = false
    while(i < sortedOps.length && !newSolFound){
      endIter = System.nanoTime() + tAvail
      val operator = sortedOps(i)
      var bestReached = false
      while(System.nanoTime() < endIter && !bestReached){
        lnsIter(operator)
        bestReached = (maximizeObjective.get && currentSol.get.objective >= bestObjective) || (!maximizeObjective.get && currentSol.get.objective <= bestObjective)
        newSolFound = (maximizeObjective.get && currentSol.get.objective > bestObjective) || (!maximizeObjective.get && currentSol.get.objective < bestObjective)
      }

      if(bestReached){
        endIter = System.nanoTime() + tAvail
        while(System.nanoTime() < endIter && !newSolFound) {
          lnsIter(operator)
          newSolFound = (maximizeObjective.get && currentSol.get.objective > bestObjective) || (!maximizeObjective.get && currentSol.get.objective < bestObjective)
        }
      }

      //Restoring initial objective:
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = Some(initSol)
      i += 1
    }

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.get.objective
    endIter = endTime
    learning = false

    if(newSolFound){
      stagnation = 0
      if(!solver.silent) println("Stagnation over")
    }
  }

  override def spotTuning(): Unit = ???

  override def alnsLoop(): Unit = {
    if(config.strategy == "alternate") alnsAlternate()
    else {
      if (!solver.silent) println("\nStarting adaptive LNS...")
      println("n operators: " + operators.length)
      stagnation = 0
      while (
        System.nanoTime() < endTime && opStore.nonActiveEmpty && !optimumFound) {
        lnsIter(opStore.select())
        if (stagnation > stagnationThreshold) onStagnation()
      }
    }
  }

  def lnsIter(operator: ALNSOperator): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + operator.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (opFunction, opFailures, opDiscrepancy) = operator.getFunction
    if(opFailures.isDefined) nFailures = opFailures.get

    var relaxDone = true
    val oldObjective = currentSol.get.objective
    val iterStart = System.nanoTime()

    val stats = solver.startSubjectTo(stopCondition, opDiscrepancy.getOrElse(Int.MaxValue), null) {
      try {
        opFunction(currentSol.get)
      }
      catch {
        case i: Inconsistency => relaxDone = false
      }
    }

    val iterEnd = System.nanoTime()
    val newObjective = currentSol.get.objective

    if(opFailures.isDefined) nFailures = 0 //Restauring failures number to 0

    val improvement = math.abs(newObjective - oldObjective)
    val time = iterEnd - iterStart

    if(improvement > 0){
      if(!learning) stagnation = 0
      if(iterTimeout >= config.timeout || time > iterTimeout) iterTimeout = time * 2
    }
    else if(!learning) stagnation += 1

    if (relaxDone) {
      //Updating probability distributions:
      operator.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = false, iter)
      if(stats.completed)
        if(!solver.silent) println("Search space completely explored, improvement: " + improvement)
      else
        if(!solver.silent) println("Search done, Improvement: " + improvement)
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement)
      operator.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = !learning, iter)
    }

    if(!operator.isInstanceOf[ALNSReifiedOperator]) {
      if (operator.isActive)
        opStore.adapt(operator)
      else {
        opStore.deactivate(operator)
        if (!solver.silent) println("Operator " + operator.name + " deactivated.")
      }
    }

    iter += 1
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators,
    maximizeObjective.isDefined & optimumFound,
    solsFound.isEmpty & optimumFound
  )

  /**
    * resets the store(s)
    */
  override def resetStore(): Unit = {
    operators.foreach(operator => {
      operator.resetFails()
      operator.setActive(true)
    })
    opStore.reset()
  }



  def alnsAlternate(): Unit = ???

  def timeLearning(): Unit = {
    operators.foreach(lnsIter)
  }

  def dive(): Unit = ???
}
