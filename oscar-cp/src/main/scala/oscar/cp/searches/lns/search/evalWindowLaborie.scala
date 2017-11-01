package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, RouletteWheel}
import oscar.cp.{CPIntVar, CPSolver}

import scala.util.Random

/**
  * TODO
  */
class EvalWindowLaborie(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config) {
  val tolerance: Double = config.metaParameters.getOrElse('tolerance, 0.5).asInstanceOf[Double]
  def evalWindow: Long = iterTimeout * 5
  override val stagnationThreshold = 10

  override lazy val relaxStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    relaxOps,
    relaxWeights.clone(),
    1.0,
    false,
    checkEfficiency
  )

  override lazy val searchStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    searchOps,
    searchWeights.clone(),
    1.0,
    false,
    checkEfficiency
  )

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0

    val t = timeInSearch

    relaxWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, relaxOps(index).name, score))
    }

    searchWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, searchOps(index).name, score))
    }

    timeLearning()

    while (
      System.nanoTime() < endTime &&
        relaxStore.nonActiveEmpty &&
        searchStore.nonActiveEmpty &&
        !optimumFound
    ) {
      val relax = relaxStore.select()
      val search = searchStore.select()
      /*if(stagnation >= stagnationThreshold && previousBest.isDefined) {
        lnsIter(relax, search, previousBest.get)
        if(search.lastExecStats.get.improvement > 0) lnsIter(relax, search)
      }
      else*/ lnsIter(relax, search)
    }
  }

  protected def timeLearning(): Unit = {
    learning = true
    iterTimeout = config.timeout
    Random.shuffle(relaxOps.toSeq).foreach(relax => {
      val search = searchOps(Random.nextInt(searchOps.length))
      lnsIter(relax, search)
    })
    Random.shuffle(searchOps.toSeq).foreach(search => {
      while(search.execs < 1) {
        val relax = relaxStore.select()
        lnsIter(relax, search)
      }
    })
    learning = false

    manageIterTimeout()
    if(!solver.silent) println("learning done, iterTimeout: " + iterTimeout)
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

      if (opDeactivation && op.time >= iterTimeout * 2 && (opEfficiency < searchEfficiency * tolerance || op.sols == 0)){
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
//        manageIterTimeout()
      }

      opEfficiency
    }
    else 1.0
  }

  protected def manageIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op => {
      op.isActive && op.execs > 0 && op.sols > 0
    }).foreach(op => {
      val avgTime = Metrics.avgTime(op)
      if (avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = if(maxTime == 0L || learning) config.timeout else maxTime
  }
}
