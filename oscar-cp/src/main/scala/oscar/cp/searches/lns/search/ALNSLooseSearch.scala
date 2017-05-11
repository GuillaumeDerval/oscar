package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator, ALNSSingleParamOperator, ALNSTwoParamsOperator}
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable
import scala.util.Random


class ALNSLooseSearch(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig)
  extends ALNSSearch(solver, vars, config) {

  //Instantiating relax operators:
  lazy val relaxOps: Array[ALNSOperator] = builder.instantiateRelaxOperators

  //Instantiating search operators:
  lazy val searchOps: Array[ALNSOperator] = builder.instantiateSearchOperators

  lazy val relaxStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(relaxOps)
  lazy val searchStore: AdaptiveStore[ALNSOperator] = builder.instantiateOperatorStore(searchOps)

  lazy val nOpCombinations: Int = relaxOps.filter(_.isActive).map(_.nParamVals).sum * searchOps.filter(_.isActive).map(_.nParamVals).sum

  //TODO: implement exponential timeout
  //TODO: add multiobjective support
  override def alnsLearning(): Unit = {
    val initSol = currentSol

    val multFactor = 2.0

    val lStart = System.nanoTime()
    var tAvail = (endTime - lStart) / nOpCombinations
    val relaxPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()
    val searchPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()

    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    }{
      val sStart = System.nanoTime()
      endSearch = Math.min(System.nanoTime() + tAvail, endTime)

      while(System.nanoTime() < endSearch && relax.isActive && search.isActive)
        lnsSearch(relax, search)

      val time = System.nanoTime() - sStart
      val improvement = Math.abs(currentSol.objective - initSol.objective)
      //TODO: adapt time and improvement bounds
//      if(model.cpObjective.best != initObj && sTime * 3 < tAvail) tAvail = sTime*3

      if(!relaxPerf.contains(relax.name)) relaxPerf += relax.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      relaxPerf(relax.name)._2 += ((time, improvement))
      if(!searchPerf.contains(search.name)) searchPerf += search.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      searchPerf(search.name)._2 += ((time, improvement))

      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = initSol
    }

    relaxPerf.values.foreach{
      case (op, perfs) =>
        if(perfs.map(_._1).min > tAvail){
          op.setActive(false)
          if(!solver.silent) println("Operator " + op.name + " deactivated")
        }
    }

    searchPerf.values.foreach{
      case (op, perfs) =>
        if(perfs.map(_._1).min > tAvail - 1000000L){
          op.setActive(false)
          if(!solver.silent) println("Operator " + op.name + " deactivated")
        }
    }

    relaxOps.filter(!_.isActive).foreach(relaxStore.remove)
    searchOps.filter(!_.isActive).foreach(searchStore.remove)

    if(!solver.silent) {
      println(relaxStore.nElements + " relax operators remaining.")
      println(searchStore.nElements + " search operators remaining.")
    }

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.objective
  }

  override def alnsLoop(): Unit = {
    while (System.nanoTime() < endTime  && relaxStore.nonEmpty && searchStore.nonEmpty)
      lnsSearch(relaxStore.select(), searchStore.select())
  }

  def lnsSearch(relax: ALNSOperator, search: ALNSOperator): Unit = {

    if(!solver.silent) println("Starting new search with: " + relax.name + " and " + search.name)

    val oldObjective = currentSol.objective

    //New search using selected strategies:
    var relaxDone = true
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
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
      if(!solver.silent) println("Search done, Improvement: " + improvement + "\n")

      //Updating probability distributions:
      relax.update(improvement, stats, fail = false)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) relaxStore.adapt(relax, metric(relax, improvement, stats))
      search.update(improvement, stats, fail = false)
      if(!relax.isInstanceOf[ALNSReifiedOperator]) searchStore.adapt(search, metric(search, improvement, stats))
    }
    else {
      if(!solver.silent) println("Search space empty, search not applied, improvement: " + improvement + "\n")

      //Updating only relax as the the search has not been done:
      relax.update(improvement, stats, fail = true)
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
