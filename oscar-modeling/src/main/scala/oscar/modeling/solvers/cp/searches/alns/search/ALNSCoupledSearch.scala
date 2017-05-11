package oscar.modeling.solvers.cp.searches.alns.search

import oscar.algo.Inconsistency
import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearchResults}
import oscar.cp.searches.lns.selection.AdaptiveStore
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ALNSCoupledSearch(md: ModelDeclaration, vars: Array[IntVar], config: ALNSConfig, xcspInstantiator: () => String)
  extends ALNSSearch(md, vars, config, xcspInstantiator) {

  //Instantiating operators:
  lazy val operators: Array[ALNSOperator] = builder.instantiateCoupledOperators

  lazy val opStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(operators)

  override def alnsLearning(): Unit = {
    val initObj = model.cpObjective.best
    val initSol = currentSol

    val lStart = System.nanoTime()
    var tAvail = ((endTime - lStart) / 2) / operators.length
    val opTime = ArrayBuffer[(ALNSOperator, Long)]()

    Random.shuffle(operators.toSeq).foreach(operator =>{
      println("Avail time: " + tAvail/1000000000.0 + "s")
      val sStart = System.nanoTime()
      endSearch = Math.min(System.nanoTime() + tAvail, endTime)

      while(System.nanoTime() < endSearch && model.cpObjective.best == initObj && operator.isActive)
        lnsSearch(operator)

      val sTime = System.nanoTime() - sStart
      opTime += ((operator, sTime))
//      if(model.cpObjective.best != initObj && sTime * 3 < tAvail) tAvail = sTime*3

      model.cpObjective.relax()
      model.cpObjective.best = initObj
      currentSol = initSol
    })

    opTime.filter(_._2 > tAvail - 1000000L).foreach(tuple => {
      val op = tuple._1
      op.setActive(false)
      opStore.remove(op)
      println("Operator " + op.name + " deactivated.")
    })

    println(opStore.nElements + " operators remaining.")

    currentSol = bestSol
    model.cpObjective.best = bestSol.objective
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime && !model.cpObjective.isOptimum() && opStore.nonEmpty)
      lnsSearch(opStore.select())
  }

  def lnsSearch(operator: ALNSOperator): Unit = {

    println("Starting new search with: " + operator.name)

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = model.cpSolver.startSubjectTo(stopCondition, Int.MaxValue, null) {
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
      operator.update(improvement, stats, fail = false)
    }

    if(operator.isActive) opStore.adapt(operator, metric(operator, improvement, stats))
    else opStore.remove(operator)
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    operators.map(x => x.name -> x.getStats).toMap
  )
}
