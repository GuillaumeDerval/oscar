package oscar.anytime.lns

import java.io.FileWriter

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
import scala.io.Source
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

    else if(argMap.getOrElse('NOLNS, false).asInstanceOf[Boolean]){
      val result = performBasicSearch(argMap, maximizeObjective)
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

    else if (argMap.getOrElse('MLNS, false).asInstanceOf[Boolean]) performMLNS(argMap, maximizeObjective, seed, argMap.getOrElse('strategy, "default").asInstanceOf[String] == "boundsOnly")

    else{
      val result = performALNS(argMap, maximizeObjective)

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
        if (result.relaxOperators.isEmpty) Map("Coupled" -> result.searchOperators) else Map("Relax" -> result.relaxOperators, "Search" -> result.searchOperators),
        result.scoreHistory
      )
    }
  }

  def performALNS(argMap: ArgMap, maximizeObjective: Boolean): ALNSSearchResults = {

    val startResults = performBasicSearch(argMap, maximizeObjective, 1)
    if(startResults.solutions.nonEmpty) {
      val startSol = startResults.solutions.head

      val coupled = argMap.getOrElse('coupled, false).asInstanceOf[Boolean]
      val fixed = argMap.getOrElse('fixed, false).asInstanceOf[Boolean]
      val opDeactivation = argMap.getOrElse('opDeactivation, false).asInstanceOf[Boolean]
      val quickStart = argMap.getOrElse('quickStart, false).asInstanceOf[Boolean]
      val altScore = argMap.getOrElse('altScore, false).asInstanceOf[Boolean]

      val useSorted = argMap.getOrElse('useSorted, false).asInstanceOf[Boolean]
      val opOrder = if(useSorted){
        val opOrderFile = "oscar-perf/src/main/scala/oscar/anytime/lns/baseline_order/" + problem + "/" + IOUtils.getFileName(instance, keepExtension = false) + ".txt"
        if(IOUtils.fileExists(opOrderFile)){
          Some(Source.fromFile(opOrderFile).getLines().map(_.split(" ")(0)).toSeq)
        }
        else None
      }
      else None

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

      val timeout = argMap.getOrElse('timeout, 0L).asInstanceOf[Long] * 1000000000L

      val startWeight = if(argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String] == ALNSBuilder.NoUpdate) 1.0 else startSol.objective.toDouble/timeout //Initial weight
//      val startWeight = 1.0

      lazy val searchStore = builder.instantiateOperatorStore(
        if(coupled) builder.instantiateCoupledOperators
        else if(fixed) builder.instantiateFixedSearchOperators
        else builder.instantiateSearchOperators,
        startWeight
      )

      lazy val relaxStore = if (coupled) new RandomStore[ALNSOperator](Array(new ALNSNoParamOperator("dummy", 0, () => (_ => Unit, None, None))))
      else if(fixed) builder.instantiateOperatorStore(builder.instantiateFixedRelaxOperators, startWeight)
      else builder.instantiateOperatorStore(builder.instantiateRelaxOperators, startWeight)

      val metaParams: Map[Symbol, Any] = Map(
        'coupled -> coupled,
        'learning -> argMap.getOrElse('learning, false).asInstanceOf[Boolean],
        'opDeactivation -> opDeactivation,
        'opOrder -> opOrder,
        'quickStart -> quickStart,
        'altScore -> altScore
      )

      val config = new ALNSConfig(
        relaxStore,
        searchStore,
        timeout,
        bestKnownObjective,
        1000,
        argMap.getOrElse('strategy, "default").asInstanceOf[String],
        metaParams
      )

      val alns = ALNSSearch(solver, decisionVariables, config)

      alns.searchFrom(startSol)
    }
    else startResults
  }

  def performMLNS(argMap: ArgMap, maximizeObjective: Boolean, seed: Int, boundsOnly: Boolean = false): Unit = {

    val startResults = performBasicSearch(argMap, maximizeObjective, 1)
    if(startResults.solutions.nonEmpty){
      val startSol = startResults.solutions.head

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
        'opDeactivation -> false,
        'stagnationThreshold -> 5000
      )

      var highestResult: Option[ALNSSearchResults] = None
      var lowestResult: Option[ALNSSearchResults] = None
      Random.shuffle(operators.toSeq).foreach(op =>{
        try {
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

          val result = alns.searchFrom(startSol)

          if (!boundsOnly) {
            XmlWriter.writeToXml(
              argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String],
              result.searchOperators.head.name,
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
          else if (result.solutions.nonEmpty) {
            if (highestResult.isEmpty || result.solutions.last.objective > highestResult.get.solutions.last.objective)
              highestResult = Some(result)
            if (lowestResult.isEmpty || result.solutions.last.objective < lowestResult.get.solutions.last.objective)
              lowestResult = Some(result)
          }
        }catch {
          case e: Exception =>
            val fw = new FileWriter(argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String] + "errors.txt", true)
            try {
              fw.write("error with operator " + op.name + " on instance " + IOUtils.getFileName(instance, keepExtension = false))
              fw.write(e.toString)
            }
            finally fw.close()
        }
      })

      if(highestResult.isDefined && lowestResult.isDefined){
        val bestResult = if(maximizeObjective) highestResult.get else lowestResult.get
        val worstResult = if(maximizeObjective) lowestResult.get else highestResult.get

        if(!solver.silent){
          println("\nBest result:")
          println(bestResult)
        }
        if(!solver.silent){
          println("\nWorst result:")
          println(worstResult)
        }

        XmlWriter.writeToXml(
          argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String],
          "Baseline_best",
          seed,
          argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
          IOUtils.getFileName(instance, keepExtension = false),
          problem,
          bestKnownObjective.getOrElse(if (maximizeObjective) Int.MinValue else Int.MaxValue),
          maximizeObjective,
          bestResult.solutions,
          if (bestResult.relaxOperators.isEmpty) Map("Coupled" -> bestResult.searchOperators) else Map("Relax" -> bestResult.relaxOperators, "Search" -> bestResult.searchOperators)
        )

        XmlWriter.writeToXml(
          argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String],
          "Baseline_worst",
          seed,
          argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
          IOUtils.getFileName(instance, keepExtension = false),
          problem,
          bestKnownObjective.getOrElse(if (maximizeObjective) Int.MinValue else Int.MaxValue),
          maximizeObjective,
          worstResult.solutions,
          if (worstResult.relaxOperators.isEmpty) Map("Coupled" -> worstResult.searchOperators) else Map("Relax" -> worstResult.relaxOperators, "Search" -> worstResult.searchOperators)
        )
      }
    }
    else Array(startResults)

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

      case "--fixed" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('fixed -> true), tail)
          else parseArgs(map ++ Map('fixed -> value.toBoolean), remTail)
        case Nil => map ++ Map('fixed -> true)
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

      case "--quick-start" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('quickStart -> true), tail)
          else parseArgs(map ++ Map('quickStart -> value.toBoolean), remTail)
        case Nil => map ++ Map('quickStart -> true)
      }

      case "--alt-score" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('altScore -> true), tail)
          else parseArgs(map ++ Map('altScore -> value.toBoolean), remTail)
        case Nil => map ++ Map('altScore -> true)
      }

      case "--selection" :: value :: tail =>
        parseArgs(map ++ Map('selection -> value), tail)

      case "--metric" :: value :: tail =>
        parseArgs(map ++ Map('metric -> value), tail)

      case "--strategy" :: value :: tail =>
        parseArgs(map ++ Map('strategy -> value), tail)

      case "--use-sorted" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('useSorted -> true), tail)
          else parseArgs(map ++ Map('useSorted -> value.toBoolean), remTail)
        case Nil => map ++ Map('useSorted -> true)
      }

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

