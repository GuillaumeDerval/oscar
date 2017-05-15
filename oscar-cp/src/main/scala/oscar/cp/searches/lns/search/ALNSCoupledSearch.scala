package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ALNSCoupledSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  //Instantiating operators:
  lazy val operators: Array[ALNSOperator] = builder.instantiateCoupledOperators

  lazy val opStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(operators)

  //TODO: implement exponential timeout
  //TODO: add multiobjective support
  override def alnsLearning(): Unit = {
    val initSol = currentSol

    val multFactor = 2.0

    val learningStart = System.nanoTime()
    var tAvail = (endTime - learningStart) / operators.length
    val opPerf = ArrayBuffer[(ALNSOperator, Long, Int)]()

    Random.shuffle(operators.toSeq).foreach(operator =>{
      if(!solver.silent) println ("Avail time: " + tAvail/1000000000.0 + "s")
      val start = System.nanoTime()
      endSearch = System.nanoTime() + tAvail

      while(System.nanoTime() < endSearch && currentSol.objective == initSol.objective && operator.isActive)
        lnsSearch(operator)

      val time = System.nanoTime() - start
      val improvement = Math.abs(currentSol.objective - initSol.objective)
      opPerf += ((operator, time, improvement))
      //TODO: update time and improvement bounds

      //Restoring initial objective:
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = initSol
    })

    opPerf.filter(_._2 > tAvail - 1000000L).foreach(tuple => {
      val op = tuple._1
      op.setActive(false)
      opStore.remove(op)
      if(!solver.silent) println("Operator " + op.name + " deactivated.")
    })

    if(!solver.silent) println(opStore.nElements + " operators remaining.")

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.objective
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime && opStore.nonEmpty)
      lnsSearch(opStore.select())
  }

  def lnsSearch(operator: ALNSOperator): Unit = {

    if(!solver.silent) println("Starting new search with: " + operator.name)

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

    if (relaxDone) {
      if(!solver.silent) println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      operator.update(improvement, stats, fail = false)
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      operator.update(improvement, stats, fail = false) //TODO:Review failure system
    }

    if(operator.isActive) opStore.adapt(operator, metric(operator, improvement, stats))
    else opStore.remove(operator)
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators.map(x => x.name -> x.getStats).toMap
  )
}
