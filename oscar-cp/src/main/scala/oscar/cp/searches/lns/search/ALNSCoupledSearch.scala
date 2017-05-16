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
    }

    if(!solver.silent) println(opStore.nElements + " operators remaining.")

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.objective
    endSearch = endTime
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
      operator.update(improvement, stats, fail = true)
    }

    if(operator.isActive) opStore.adapt(operator, metric(operator, improvement, stats))
    else{
      if(!solver.silent) println("Operator " + operator.name + " deactivated.")
      opStore.remove(operator)
    }
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators.map(x => x.name -> x.getStats).toMap
  )
}
