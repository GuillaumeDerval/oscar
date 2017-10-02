package oscar.cp.searches.lns.search

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSElement, ALNSOperator}
import oscar.cp.searches.lns.selection.{Metrics, PriorityStore, RouletteWheel}

import scala.util.Random

/**
  * TODO
  */
class ALNSDiveAndExplore(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config){
  val minEfficiencyThreshold = 0.001
  val efficiencyEvalTime = 10000000000L

  override def alnsLoop(): Unit = {
    if (!solver.silent){
      println("\nStarting adaptive LNS...")
      println("n operators: " + nOpCombinations)
    }
    if(!solver.silent) println("\nStarting dive...")
    dive()
    if(!solver.silent) println("\nStagnation, starting exploration")
    explore()
  }

  def timeLearning(): Unit = {
  }

  private def dive(): Unit = {
    //Running each op combination to evaluate TTI:
    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    } {
      lnsIter(relax, search)
    }

    //Diving:
    val relaxPriority = new PriorityStore[ALNSOperator](relaxOps, relaxOps.map(Metrics.timeToImprovement), 1.0, true, Metrics.timeToImprovement)
    val searchPriority = new PriorityStore[ALNSOperator](searchOps, searchOps.map(Metrics.timeToImprovement), 1.0, true, Metrics.timeToImprovement)
    while(System.nanoTime() < endTime && relaxPriority.nonActiveEmpty && searchPriority.nonActiveEmpty && !optimumFound){
      val relax = relaxPriority.select()
      val search = searchPriority.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      checkEfficiency(search)
      if(relax.lastExecStats.improvement == 0){
        if(relax.name != "dummy") relaxPriority.deactivate(relax)
        searchPriority.deactivate(search)
      }
      else{
        relaxPriority.reset()
        searchPriority.reset()
      }
    }
  }

  private def explore(): Unit = {
    val relaxRWheel = new RouletteWheel[ALNSOperator](relaxOps.filter(_.isActive), relaxOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    val searchRWheel = new RouletteWheel[ALNSOperator](searchOps.filter(_.isActive), searchOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    while(System.nanoTime() < endTime && relaxRWheel.nonActiveEmpty && searchRWheel.nonActiveEmpty && !optimumFound){
      val relax = relaxRWheel.select()
      val search = searchRWheel.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      if(!relax.isActive) relaxRWheel.deactivate(relax)
      checkEfficiency(search)
      if(!search.isActive) searchRWheel.deactivate(search)
    }
  }

  private def multiArmedBandit(elem: ALNSElement): Double = {
    elem.efficiency(efficiencyEvalTime) + Math.sqrt((2 * Math.log(iter))/elem.execs)
  }

  private def checkEfficiency(op: ALNSOperator): Unit = {
    if(op.name != "dummy" && op.time >= efficiencyEvalTime && op.efficiency(efficiencyEvalTime) >= minEfficiencyThreshold){
      op.setActive(false)
      if(!solver.silent) println("\nOperator " + op.name + " deactivated due to low efficiency!")
    }
  }
}
