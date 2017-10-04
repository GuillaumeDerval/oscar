package oscar.anytime.lns

import oscar.algo.Inconsistency
import oscar.algo.search.{Branching, DFSearch}
import oscar.anytime.lns.utils.{IOUtils, XmlWriter}
import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators._
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch, ALNSSearchResults}
import oscar.cp.searches.lns.selection.RandomStore

import scala.collection.mutable
import scala.util.Random

trait Benchmark {

  def solver: CPSolver
  def decisionVariables: Array[CPIntVar]
  def bestKnownObjective: Option[Int] = None
  def instance: String
  def problem: String

  type ArgMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {

    val argMap = parseArgs(Map(), args.toList)
    if(argMap.nonEmpty && !argMap.contains('name))
      println("WARNING: A config name should be provided if using custom parameters!")

//    println("args:\n" + argMap.mkString("\n") + "\n")

    solver.silent = !argMap.getOrElse('verbose, false).asInstanceOf[Boolean]

    val seed = argMap.getOrElse('seed, Random.nextInt(Int.MaxValue)).asInstanceOf[Int]
    if(!solver.silent) println("Seed: " + seed)
    Random.setSeed(seed)

    val maximizeObjective: Boolean = solver.objective.objs.head.isMax

    if(argMap.getOrElse('XP, false).asInstanceOf[Boolean]) performXP(argMap)
    else {
      val result =
        if (argMap.getOrElse('NOLNS, false).asInstanceOf[Boolean]) performBasicSearch(argMap, maximizeObjective)
        else if (argMap.getOrElse('MLNS, false).asInstanceOf[Boolean]) performMLNS(argMap, maximizeObjective)
        else performALNS(argMap)

      XmlWriter.writeToXml(
        argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String],
        argMap.getOrElse('name, "default").asInstanceOf[String],
        seed,
        argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
        IOUtils.getFileName(instance, keepExtension = false),
        problem,
        bestKnownObjective.getOrElse(if (maximizeObjective) Int.MinValue else Int.MaxValue),
        maximizeObjective,
        result.solutions,
        if (result.relaxOperators.isEmpty) Map("Coupled" -> result.searchOperators) else Map("Relax" -> result.relaxOperators, "Search" -> result.searchOperators)
      )
    }
  }

  def performALNS(argMap: ArgMap): ALNSSearchResults = {

    val coupled = argMap.getOrElse('coupled, false).asInstanceOf[Boolean]
    val opDeactivation = argMap.getOrElse('opDeactivation, false).asInstanceOf[Boolean]

    val builder = new ALNSBuilder(
      solver,
      decisionVariables,

      argMap.getOrElse(
        'relax,
        Array(ALNSBuilder.RandomRelax, ALNSBuilder.KSuccessiveRelax, ALNSBuilder.PropGuidedRelax, ALNSBuilder.RevPropGuidedRelax, ALNSBuilder.FullRelax) //(Reversed) propagation guided may cause out of memory on big instances
      ).asInstanceOf[Array[String]],

      argMap.getOrElse(
        'search,
        Array(ALNSBuilder.ConfOrderSearch, ALNSBuilder.FirstFailSearch, ALNSBuilder.LastConfSearch, ALNSBuilder.ExtOrientedSearch, ALNSBuilder.WeightDegSearch)
      ).asInstanceOf[Array[String]],

      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],

      argMap.getOrElse('relaxSize, ALNSBuilder.DefRelaxParam).asInstanceOf[Array[Double]],
      argMap.getOrElse('nFailures, ALNSBuilder.DefNFailures).asInstanceOf[Array[Int]],

      argMap.getOrElse('valLearn, false).asInstanceOf[Boolean],
      opDeactivation
    )

    lazy val searchStore = builder.instantiateOperatorStore(
      if(coupled) builder.instantiateCoupledOperators
      else builder.instantiateSearchOperators
    )

    lazy val relaxStore = if(coupled) new RandomStore[ALNSOperator](Array(new ALNSNoParamOperator("dummy", 0, () => (_ => Unit, None, None))))
    else builder.instantiateOperatorStore(builder.instantiateRelaxOperators)

    val metaParams: Map[Symbol, Any] = Map(
      'coupled -> coupled,
      'learning -> argMap.getOrElse('learning, false).asInstanceOf[Boolean],
      'opDeactivation -> opDeactivation
    )

    val config = new ALNSConfig(
      relaxStore,
      searchStore,
      argMap.getOrElse('timeout, 0L).asInstanceOf[Long] * 1000000000L,
      bestKnownObjective,
      1000,
      argMap.getOrElse('strategy, "default").asInstanceOf[String],
      metaParams
    )

    val alns = ALNSSearch(solver, decisionVariables, config)

    alns.search()
  }

  def performMLNS(argMap: ArgMap, maximizeObjective: Boolean): ALNSSearchResults = {

    val builder = new ALNSBuilder(
      solver,
      decisionVariables,

      argMap.getOrElse(
        'relax,
        Array(ALNSBuilder.RandomRelax, ALNSBuilder.KSuccessiveRelax, ALNSBuilder.PropGuidedRelax, ALNSBuilder.RevPropGuidedRelax, ALNSBuilder.FullRelax) //(Reversed) propagation guided may cause out of memory on big instances
      ).asInstanceOf[Array[String]],

      argMap.getOrElse(
        'search,
        Array(ALNSBuilder.ConfOrderSearch, ALNSBuilder.FirstFailSearch, ALNSBuilder.LastConfSearch, ALNSBuilder.ExtOrientedSearch, ALNSBuilder.WeightDegSearch)
      ).asInstanceOf[Array[String]],

      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],

      argMap.getOrElse('relaxSize, ALNSBuilder.DefRelaxParam).asInstanceOf[Array[Double]],
      argMap.getOrElse('nFailures, ALNSBuilder.DefNFailures).asInstanceOf[Array[Int]],

      argMap.getOrElse('valLearn, false).asInstanceOf[Boolean],
      false
    )

    lazy val operators = builder.instantiateCoupledOperators

    val metaParams: Map[Symbol, Any] = Map(
      'coupled -> true,
      'learning -> false,
      'opDeactivation -> false
    )

    val startResults = performBasicSearch(argMap, maximizeObjective, 1)
    if(startResults.solutions.nonEmpty){
      val startSol = performBasicSearch(argMap, maximizeObjective, 1).solutions.head

      val results = mutable.ArrayBuffer[ALNSSearchResults]()
      operators.foreach(op =>{
        lazy val relaxStore = new RandomStore[ALNSOperator](Array(new ALNSNoParamOperator("dummy", 0, () => (_ => Unit, None, None))))
        lazy val searchStore = new RandomStore[ALNSOperator](Array(op))

        val config = new ALNSConfig(
          relaxStore,
          searchStore,
          argMap.getOrElse('timeout, 0L).asInstanceOf[Long] * 1000000000L,
          bestKnownObjective,
          1000,
          "default",
          metaParams
        )
        val alns = ALNSSearch(solver, decisionVariables, config)

        results += alns.searchFrom(startSol)
      })

      val bestResult = if(maximizeObjective) results.filter(_.solutions.nonEmpty).maxBy(_.solutions.last.objective)
      else results.filter(_.solutions.nonEmpty).minBy(_.solutions.last.objective)
      if(!solver.silent){
        println("\nBest result:")
        println(bestResult)
      }
      bestResult
    }
    else startResults
  }

  def performBasicSearch(argMap: ArgMap, maximizeObjective: Boolean, nSols: Int = 0): ALNSSearchResults = {
    val startTime: Long = System.nanoTime()
    val endTime: Long = startTime + (argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L)

    if(!solver.silent) println("Objective type: " + (if(maximizeObjective) "max" else "min"))

    val solsFound = new mutable.ListBuffer[CPIntSol]()

    solver.onSolution{
      val time = System.nanoTime() - startTime
      solsFound += new CPIntSol(decisionVariables.map(_.value), solver.objective.objs.head.best, time)
    }

    val stopCondition: (DFSearch) => Boolean = (s: DFSearch) =>{
      var stop = false
      stop |= System.nanoTime() >= endTime
      stop |= (nSols > 0 && s.nSolutions >= nSols)
      stop |= (bestKnownObjective.isDefined && maximizeObjective && solver.objective.objs.head.best >= bestKnownObjective.get)
      stop |= (bestKnownObjective.isDefined && !maximizeObjective && solver.objective.objs.head.best <= bestKnownObjective.get)
      stop
    }

    if(!solver.silent) println("Starting search...")
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(SearchFunctions.conflictOrdering(decisionVariables, if(maximizeObjective) "Min" else "Max", valLearn = true))
    }

    if(!solver.silent) println("Search done, retrieving results")
    new ALNSSearchResults(solsFound.toArray, stats.completed || solver.objective.isOptimum(), false)
  }

  def parseArgs(map : ArgMap, list: List[String]) : ArgMap = {

    def isSwitch(s : String) = s(0) == '-'

    list match {
      case Nil => map

      case "--seed" :: value :: tail =>
        parseArgs(map ++ Map('seed -> value.toInt), tail)

      case "--verbose" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('verbose -> true), tail)
          else parseArgs(map ++ Map('verbose -> value.toBoolean), remTail)
        case Nil => map ++ Map('verbose -> true)
      }

      case "--timeout" :: value :: tail =>
        parseArgs(map ++ Map('timeout -> value.toLong), tail)

      case "--name" :: value :: tail =>
        parseArgs(map ++ Map('name -> value), tail)

      case "--out" :: value :: tail =>
        parseArgs(map ++ Map('out -> value), tail)

      case "--NOLNS" :: tail =>
        parseArgs(map ++ Map('NOLNS -> true), tail)

      case "--MLNS" :: tail =>
        parseArgs(map ++ Map('MLNS -> true), tail)

      case "--XP" :: tail =>
        parseArgs(map ++ Map('XP -> true), tail)

      case "--coupled" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('coupled -> true), tail)
          else parseArgs(map ++ Map('coupled -> value.toBoolean), remTail)
        case Nil => map ++ Map('coupled -> true)
      }

      case "--learning" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('learning -> true), tail)
          else parseArgs(map ++ Map('learning -> value.toBoolean), remTail)
        case Nil => map ++ Map('learning -> true)
      }

      case "--relax" :: value :: tail if !isSwitch(value) =>
        val relax = mutable.ListBuffer[String]()
        var next = list.tail
        while(next.nonEmpty && !isSwitch(next.head)){
          relax += next.head
          next = next.tail
        }
        parseArgs(map ++ Map('relax -> relax.toArray), next)

      case "--relax-size" :: value :: tail if !isSwitch(value) =>
        val relaxSize = mutable.ListBuffer[Double]()
        var next = list.tail
        while(next.nonEmpty && !isSwitch(next.head)){
          relaxSize += next.head.toDouble
          next = next.tail
        }
        parseArgs(map ++ Map('relaxSize -> relaxSize.toArray), next)

      case "--search" :: value :: tail if !isSwitch(value) =>
        val search = mutable.ListBuffer[String]()
        var next = list.tail
        while(next.nonEmpty && !isSwitch(next.head)){
          search += next.head
          next = next.tail
        }
        parseArgs(map ++ Map('search -> search.toArray), next)

      case "--n-failures" :: value :: tail if !isSwitch(value) =>
        val nFailures = mutable.ListBuffer[Int]()
        var next = list.tail
        while(next.nonEmpty && !isSwitch(next.head)){
          nFailures += next.head.toInt
          next = next.tail
        }
        parseArgs(map ++ Map('nFailures -> nFailures.toArray), next)

      case "--val-learn" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('valLearn -> true), tail)
          else parseArgs(map ++ Map('valLearn -> value.toBoolean), remTail)
        case Nil => map ++ Map('valLearn -> true)
      }

      case "--op-deactivation" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('opDeactivation -> true), tail)
          else parseArgs(map ++ Map('opDeactivation -> value.toBoolean), remTail)
        case Nil => map ++ Map('opDeactivation -> true)
      }

      case "--selection" :: value :: tail =>
        parseArgs(map ++ Map('selection -> value), tail)

      case "--metric" :: value :: tail =>
        parseArgs(map ++ Map('metric -> value), tail)

      case "--strategy" :: value :: tail =>
        parseArgs(map ++ Map('strategy -> value), tail)

      case option :: tail =>
        if(isSwitch(option) && tail.isEmpty) println("Option " + option + " has no value")
        else println("Unknown option " + option)
        sys.exit(1)
    }
  }

  /**
    * Experiments:
    */

  def performXP(argMap: ArgMap): Unit = {

//    val coupled = argMap.getOrElse('coupled, false).asInstanceOf[Boolean]
//    val opDeactivation = argMap.getOrElse('opDeactivation, false).asInstanceOf[Boolean]
//
//    val builder = new ALNSBuilder(
//      solver,
//      decisionVariables,
//
//      argMap.getOrElse(
//        'relax,
//        Array(ALNSBuilder.RandomRelax) //(Reversed) propagation guided may cause out of memory on big instances
//      ).asInstanceOf[Array[String]],
//
//      argMap.getOrElse(
//        'search,
//        Array(ALNSBuilder.ConfOrderSearch)
//      ).asInstanceOf[Array[String]],
//
//      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
//      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
//      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
//      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
//
//      argMap.getOrElse('relaxSize, ALNSBuilder.DefRelaxParam).asInstanceOf[Array[Double]],
//      argMap.getOrElse('nFailures, ALNSBuilder.DefNFailures).asInstanceOf[Array[Int]],
//
//      argMap.getOrElse('valLearn, false).asInstanceOf[Boolean],
//      opDeactivation
//    )

    val params = tuneParameters(0, Array(), Array())

//    lazy val operator: ALNSOperator = ???
//    lazy val searchStore = builder.instantiateOperatorStore(Array(operator))
//
//    lazy val relaxStore = None
//
//    val config = new ALNSConfig(
//      argMap.getOrElse('timeout, 0L).asInstanceOf[Long] * 1000000000L,
//      bestKnownObjective,
//      1000,
//      true,
//      false,
//      relaxStore,
//      searchStore,
//      opDeactivation,
//      builder.instantiateMetric()
//    )
//
//    val stats = solver.startSubjectTo((s: DFSearch) => s.nSolutions >=1, Int.MaxValue, null){
//      solver.onSolution{
//        currentSol = Some(new CPIntSol(decisionVariables.map(_.value), solver.objective.objs.head.best, 0L))
//      }
//      solver.search(SearchFunctions.conflictOrdering(decisionVariables, "Max", valLearn = false))
//    }
//
//    val alns = ALNSSearch(solver, decisionVariables, config)
//
//    alns.searchFrom(currentSol.get)
  }

  def tuneParameters(startObjective: Int, relaxSize: Array[Double], nFailures: Array[Int]): (Double, Int) = {
    ???
//    val R = org.ddahl.rscala.RClient() //Instantiating R client
//
//    //Setting up packages:
//    R eval """
//          spot.ok <- require('SPOT')
//          if(!spot.ok){
//            install.packages('SPOT')
//            spot.ok <- require('SPOT')
//          }
//           """
//    if(R.getL0("spot.ok")){
//
//      //TODO
//
//      (0.0, 0)
//    } else{
//      throw new Exception("Unable to load SPOT!")
//    }
  }

  val searchTimeout = 10000000000L
  var currentSol: Option[CPIntSol] = None
  lazy val searchBranching: Branching = SearchFunctions.conflictOrdering(decisionVariables, "Max", valLearn = false)

  def performSearch(relaxSize: Double, nFailures: Int): Int = {
    val endTime = System.nanoTime() + searchTimeout
    val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
      var stop = false
      stop |= nFailures != 0 && s.nBacktracks >= nFailures
      stop |= System.nanoTime() >= endTime
      stop
    }

    var relaxDone = true
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
      try {
        RelaxationFunctions.randomRelax(solver, decisionVariables, currentSol.get, Math.ceil(decisionVariables.length * relaxSize).toInt)
      }
      catch {
        case i: Inconsistency => relaxDone = false
      }
      if(relaxDone) searchBranching
    }

    val objective = solver.objective.objs.head.best
    solver.objective.objs.head.relax()
    solver.objective.objs.head.best = currentSol.get.objective
    objective
  }
}

