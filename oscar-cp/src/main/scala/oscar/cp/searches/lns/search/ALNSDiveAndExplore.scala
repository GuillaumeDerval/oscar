package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSElement, ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.{Metrics, PriorityStore, RouletteWheel}

import scala.util.Random

/**
  * TODO
  */
class ALNSDiveAndExplore(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config){
//  lazy val efficiencyEvalTime: Long = iterTimeout * 2 //(config.timeout/nOpCombinations.toDouble).ceil.toLong
//  lazy val minEfficiencyThreshold: Double = 0.5 //Math.abs(bestSol.get.objective - previousBest.get.objective) / (efficiencyEvalTime.toDouble * 10)
  lazy val tolerance = 0.5
  lazy val efficiencyEvalIters = 5

  override def alnsLoop(): Unit = {
    if (!solver.silent){
      println("\nStarting adaptive LNS...")
      println("n operators: " + nOpCombinations)
//      println("Efficiency Threshold: " + minEfficiencyThreshold)
      println("tolerance: " + tolerance)
    }
    if(!solver.silent) println("\nStarting dive...")
    dive()
    if(!solver.silent) println("\nStagnation, starting exploration")
    explore()
  }

  def timeLearning(): Unit = {
  }

  protected def dive(): Unit = {
    //Running each op combination to evaluate TTI:
    learning = true
    iterTimeout = config.timeout
    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    } {
      lnsIter(relax, search)
    }
    learning = false

    manageIterTimeout()
    println("learning done, iterTimeout: " + iterTimeout)

    //Diving:
    val relaxPriority = new PriorityStore[ALNSOperator](relaxOps, relaxOps.map(Metrics.timeToImprovement), 1.0, true, Metrics.timeToImprovement)
    val searchPriority = new PriorityStore[ALNSOperator](searchOps, searchOps.map(Metrics.timeToImprovement), 1.0, true, Metrics.timeToImprovement)
    while(System.nanoTime() < endTime && relaxPriority.nonActiveEmpty && searchPriority.nonActiveEmpty && !optimumFound){
      val relax = relaxPriority.select()
      val search = searchPriority.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      checkEfficiency(search)
      val execStats = relax.lastExecStats
      if(execStats.isEmpty || execStats.get.improvement == 0){
        if(relax.name != "dummy") relaxPriority.deactivate(relax)
        searchPriority.deactivate(search)
      }
      else{
        relaxPriority.reset()
        searchPriority.reset()
      }
    }
  }

  protected def explore(): Unit = {
    val relaxRWheel = new RouletteWheel[ALNSOperator](relaxOps.filter(_.isActive), relaxOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    val searchRWheel = new RouletteWheel[ALNSOperator](searchOps.filter(_.isActive), searchOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    while(System.nanoTime() < endTime && relaxRWheel.nonActiveEmpty && searchRWheel.nonActiveEmpty && !optimumFound){
      val relax = relaxRWheel.select()
      val search = searchRWheel.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      if(!relax.isActive){
        relaxRWheel.deactivate(relax)
        manageIterTimeout()
      }
      checkEfficiency(search)
      if(!search.isActive){
        searchRWheel.deactivate(search)
        manageIterTimeout()
      }
    }
  }

  protected def multiArmedBandit(elem: ALNSElement): Double = {
    elem.efficiency(iterTimeout) + Math.sqrt((2 * Math.log(iter))/elem.execs)
  }

  protected def checkEfficiency(op: ALNSOperator): Unit = {
    if(op.name != "dummy" && op.time >= iterTimeout) {
      val efficiency = op.efficiency(iterTimeout * efficiencyEvalIters)
      if(!solver.silent) println("Operator " + op.name + " efficiency is " + efficiency)
      if (efficiency < searchEfficiency(iterTimeout * efficiencyEvalIters) * tolerance) {
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
        manageIterTimeout()
      }
    }
  }

  protected def searchEfficiency(t: Long): Double = {
    if(solsFound.isEmpty) 0.0
    else {
      val currentTime = System.nanoTime()
      val currentObj = solsFound.last.objective
      var tStart = currentTime
      var objStart = currentObj
      val sols = solsFound.reverseIterator
      while (sols.hasNext && currentTime - tStart < t) {
        val nextSol = sols.next()
        tStart = nextSol.time
        objStart = nextSol.objective
      }
      Math.abs(objStart - currentObj).toDouble / (Math.abs(currentTime - tStart) / 1000000000.0)
    }
  }

  protected def manageIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op =>{ op.isActive && op.execs > 0}).foreach(op =>{
      val avgTime = Metrics.avgTime(op)
      if(avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = maxTime
  }

  override protected def lnsIter(relax: ALNSOperator, search: ALNSOperator): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (relaxFunction, _, _) = relax.getFunction
    val (searchFunction, searchFailures, searchDiscrepancy) = search.getFunction
    if(searchFailures.isDefined) nFailures = searchFailures.get

    do {
      var relaxDone = true
      val oldObjective = currentSol.get.objective
      val iterStart = System.nanoTime()

      val stats = solver.startSubjectTo(stopCondition, searchDiscrepancy.getOrElse(Int.MaxValue), null) {
        try {
          relaxFunction(currentSol.get)
          searchFunction(currentSol.get)
        }
        catch {
          case _: Inconsistency => relaxDone = false
        }
      }

      val iterEnd = System.nanoTime()
      val newObjective = currentSol.get.objective

      if(searchFailures.isDefined) nFailures = 0 //Resetting failures number to 0

      val improvement = math.abs(currentSol.get.objective - oldObjective)
      val time = iterEnd - iterStart

      if(improvement > 0) stagnation = 0
      else stagnation += 1

      if (!solver.silent){
        if(!relaxDone) println("Search space empty, search not applied, improvement: " + improvement)
        else if(stats.completed) println("Search space completely explored, improvement: " + improvement)
        else println("Search done, Improvement: " + improvement)
      }

      //Updating probability distributions:
      relax.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = !relaxDone && !learning, iter)
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        if (relax.isActive) relaxStore.adapt(relax)
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) relaxStore.deactivate(relax)
        }
      }

      if(relaxDone || relax.name == "dummy"){
        search.update(iterStart, iterEnd, oldObjective, newObjective, stats, fail = false, iter)
        if(!search.isInstanceOf[ALNSReifiedOperator]){
          if (search.isActive) searchStore.adapt(search)
          else {
            if (!solver.silent) println("Operator " + search.name + " deactivated")
            if (!learning) searchStore.deactivate(search)
          }
        }
      }
    }while(System.nanoTime() < endIter && !learning)

    iter += 1
  }
}