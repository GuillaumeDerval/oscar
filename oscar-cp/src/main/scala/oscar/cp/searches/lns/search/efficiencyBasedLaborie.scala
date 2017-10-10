package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, RouletteWheel}
import oscar.cp.{CPIntVar, CPSolver}

import scala.util.Random

/**
  * TODO
  */
class EfficiencyBasedLaborie(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config) {
  lazy val tolerance = 0.5
  def evalWindow: Long = iterTimeout * 5

  override lazy val relaxOps: Array[ALNSOperator] = config.relaxStore.getElements.toArray
  override lazy val searchOps: Array[ALNSOperator] = config.searchStore.getElements.toArray

  override lazy val relaxStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    relaxOps,
    1.0,
    false,
    checkEfficiency
  )

  override lazy val searchStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    searchOps,
    1.0,
    false,
    checkEfficiency
  )

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0

    timeLearning()

    while (
      System.nanoTime() < endTime &&
        relaxStore.nonActiveEmpty &&
        searchStore.nonActiveEmpty &&
        !optimumFound
    ) {
      val relax = relaxStore.select()
      val search = searchStore.select()
      lnsIter(relax, search)
    }
  }

  def timeLearning(): Unit = {
    learning = true
    iterTimeout = config.timeout
    for {
      relax <- Random.shuffle(relaxStore.getElements)
      search <- Random.shuffle(searchStore.getElements)
    } {
      lnsIter(relax, search)
    }
    learning = false

    manageIterTimeout()
    println("learning done, iterTimeout: " + iterTimeout)
  }

  protected def checkEfficiency(op: ALNSOperator): Double = {
    if(op.name != "dummy"){
      val now = timeInSearch
      val tWindowStart = Math.min(now - evalWindow, if (solsFound.nonEmpty) solsFound.last.time else 0L)
      val opEfficiency = Metrics.efficiencySince(op, tWindowStart)
      val searchEfficiency = Metrics.searchEfficiencySince(solsFound, tWindowStart, now)

      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + op.name + " efficiency is " + opEfficiency)
      }

      if (opEfficiency < searchEfficiency * tolerance && op.time >= iterTimeout) {
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
        manageIterTimeout()
      }

      opEfficiency
    }
    else 0.0
  }

  protected def manageIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op => {
      op.isActive && op.execs > 0
    }).foreach(op => {
      val avgTime = Metrics.avgTime(op)
      if (avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = maxTime
  }
}
