package oscar.modeling.solvers.cp.searches.alns.search

import oscar.algo.Inconsistency
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator, ALNSSingleParamOperator, ALNSTwoParamsOperator}
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearchResults}
import oscar.cp.searches.lns.selection.AdaptiveStore
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

import scala.collection.mutable
import scala.util.Random


class ALNSLooseSearch(md: ModelDeclaration, vars: Array[IntVar], config: ALNSConfig, xcspInstantiator: () => String)
  extends ALNSSearch(md, vars, config, xcspInstantiator) {

  //Instantiating relax operators:
  lazy val relaxOps: Array[ALNSOperator] = builder.instantiateRelaxOperators

  //Instantiating search operators:
  lazy val searchOps: Array[ALNSOperator] = builder.instantiateSearchOperators

  lazy val relaxStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(relaxOps)
  lazy val searchStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(searchOps)

  lazy val nOpCombinations: Int = relaxOps.filter(_.isActive).map(_.nParamVals).sum * searchOps.filter(_.isActive).map(_.nParamVals).sum

  var learning = false

  override def alnsLearning(): Unit = {
    learning = true
    val initObj = model.cpObjective.best
    val initSol = currentSol

    val lStart = System.nanoTime()
    var tAvail = ((endTime - lStart) / 2) / nOpCombinations
    val relaxTime = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[Long])]()
    val searchTime = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[Long])]()

    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    }{
      val sStart = System.nanoTime()
      endSearch = Math.min(System.nanoTime() + tAvail, endTime)

      while(System.nanoTime() < endSearch && model.cpObjective.best == initObj && relax.isActive && search.isActive)
        lnsSearch(relax, search)

      val sTime = System.nanoTime() - sStart
//      if(model.cpObjective.best != initObj && sTime * 3 < tAvail) tAvail = sTime*3

      if(!relaxTime.contains(relax.name)) relaxTime += relax.name -> (relax, mutable.ArrayBuffer[Long]())
      relaxTime(relax.name)._2 += sTime
      if(!searchTime.contains(search.name)) searchTime += search.name -> (relax, mutable.ArrayBuffer[Long]())
      searchTime(search.name)._2 += sTime
      model.cpObjective.relax()
      model.cpObjective.best = initObj
      currentSol = initSol
    }

    relaxTime.values.foreach{
      case (op, execTimes) =>
        if(execTimes.min > tAvail){
          op.setActive(false)
          println("Operator " + op.name + " deactivated")
        }
    }

    searchTime.values.foreach{
      case (op, execTimes) =>
        if(execTimes.min > tAvail - 1000000L){
          op.setActive(false)
          println("Operator " + op.name + " deactivated")
        }
    }

    relaxOps.filter(!_.isActive).foreach(relaxStore.remove)
    searchOps.filter(!_.isActive).foreach(searchStore.remove)


    println(relaxStore.nElements + " relax operators remaining.")
    println(searchStore.nElements + " search operators remaining.")

    currentSol = bestSol
    model.cpObjective.best = bestSol.objective
    learning = false
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime && !model.cpObjective.isOptimum() && relaxStore.nonEmpty && searchStore.nonEmpty)
      lnsSearch(relaxStore.select(), searchStore.select())
  }

  def lnsSearch(relax: ALNSOperator, search: ALNSOperator): Unit = {

    println("Starting new search with: " + relax.name + " and " + search.name)

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = model.cpSolver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        relax(currentSol)
      }
      catch {
        case _: Inconsistency => relaxDone = false
      }
      if (relaxDone) search(currentSol)
    }

    val improvement = math.abs(currentSol.objective - oldObjective)

    if (relaxDone) {
      println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      relax.update(improvement, stats, fail = false)
      //TODO: do something in case of learning
      if(!relax.isInstanceOf[ALNSReifiedOperator]) relaxStore.adapt(relax, metric(relax, improvement, stats))
      search.update(improvement, stats, fail = false)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) searchStore.adapt(search, metric(search, improvement, stats))
    }
    else {
      println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      relax.update(improvement, stats, fail = false)
      if(relax.isActive && !relax.isInstanceOf[ALNSReifiedOperator]) relaxStore.adapt(relax, metric(relax, improvement, stats))
      else relaxStore.remove(relax)
    }
  }

  override def getSearchResults = new ALNSSearchResults(
    solsFound.toArray,
    relaxOps.map(x => x.name -> x.getStats).toMap,
    searchOps.map(x => x.name -> x.getStats).toMap
  )

  private def getReifiedOperators(operators: Seq[ALNSOperator]) = operators.flatMap{
    case op: ALNSSingleParamOperator[_] => op.getReifiedParameters
    case op: ALNSTwoParamsOperator[_,_] => op.getReifiedParameters
    case op: ALNSOperator => Seq(op)
  }
}
