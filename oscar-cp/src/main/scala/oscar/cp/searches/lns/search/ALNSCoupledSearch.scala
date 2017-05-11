package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.CPIntVar
import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ALNSCoupledSearch(vars: Array[CPIntVar], objective: CPIntVar, config: ALNSConfig)
  extends ALNSSearch(vars, objective, config) {

  //Instantiating operators:
  lazy val operators: Array[ALNSOperator] = builder.instantiateCoupledOperators

  lazy val opStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(operators)

  //TODO: implement exponential timeout
  override def alnsLearning(): Unit = {
    val initObj = currentSol.objective
    val initSol = currentSol

    val multFactor = 2.0

    val learningStart = System.nanoTime()
    var tAvail = (endTime - learningStart) / operators.length
    val opPerf = ArrayBuffer[(ALNSOperator, Long, Int)]()

    Random.shuffle(operators.toSeq).foreach(operator =>{
      println("Avail time: " + tAvail/1000000000.0 + "s")
      val start = System.nanoTime()
      endSearch = System.nanoTime() + tAvail

      while(System.nanoTime() < endSearch && currentSol.objective == initObj && operator.isActive)
        lnsSearch(operator)

      val time = System.nanoTime() - start
      val improvement = Math.abs(currentSol.objective - initObj)
      opPerf += ((operator, time, improvement))
      //TODO: adapt time and improvement bounds

      //TODO: restore initial solution bounds
//      model.cpObjective.relax()
//      model.cpObjective.best = initObj
      currentSol = initSol
    })

    opPerf.filter(_._2 > tAvail - 1000000L).foreach(tuple => {
      val op = tuple._1
      op.setActive(false)
      opStore.remove(op)
      println("Operator " + op.name + " deactivated.")
    })

    println(opStore.nElements + " operators remaining.")

    currentSol = bestSol
    //TODO: set best sol bounds
//    model.cpObjective.best = bestSol.objective
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime && opStore.nonEmpty)
      lnsSearch(opStore.select())
  }

  def lnsSearch(operator: ALNSOperator): Unit = {

    println("Starting new search with: " + operator.name)

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = cp.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        operator(currentSol)
      }
      catch {
        case i: Inconsistency => relaxDone = false
      }
    }

    val improvement = math.abs(currentSol.objective - oldObjective)

    if (relaxDone) {
      println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      operator.update(improvement, stats, fail = false)
    }
    else {
      println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      operator.update(improvement, stats, fail = true)
    }

    if(operator.isActive) opStore.adapt(operator, metric(operator, improvement, stats))
    else opStore.remove(operator)
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators.map(x => x.name -> x.getStats).toMap
  )
}
