package oscar.anytime.lns

import oscar.algo.search.DFSearch
import oscar.anytime.lns.utils.{IOUtils, XmlWriter}
import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSBuilder, SearchFunctions}
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch, ALNSSearchResults}

import scala.collection.mutable
import scala.util.Random

trait Benchmark {

  def solver: CPSolver
  def decisionVariables: Array[CPIntVar]
  def bestKnownObjective: Int = Int.MaxValue
  def instance: String
  def problem: String

  type ArgMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {

    val argMap = parseArgs(Map(), args.toList)
    if(argMap.nonEmpty && !argMap.contains('name))
      println("WARNING: A config name should be provided if using custom parameters!")

//    println("args:\n" + argMap.mkString("\n") + "\n")

    val result = if(argMap.getOrElse('NOLNS, false).asInstanceOf[Boolean]) performBasicSearch(argMap) else performALNS(argMap)

    XmlWriter.writeToXml(
      argMap.getOrElse('out, "ALNS-bench-results/Tests/").asInstanceOf[String],
      argMap.getOrElse('name, "default").asInstanceOf[String],
      argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
      IOUtils.getFileName(instance, keepExtension = false),
      problem,
      bestKnownObjective,
      solver.objective.objs.head.isMax,
      result.solutions
    )
  }

  def performALNS(argMap: ArgMap): ALNSSearchResults = {
    val seed = Random.nextInt()
    println("Seed: " + seed)
    Random.setSeed(seed)
    val config = new ALNSConfig(
      timeout = argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
      coupled = argMap.getOrElse('coupled, false).asInstanceOf[Boolean],
      learning = argMap.getOrElse('learning, false).asInstanceOf[Boolean],

      argMap.getOrElse(
        'relax,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided) //(Reversed) propagation guided may cause out of memory on big instances
      ).asInstanceOf[Array[String]],

      argMap.getOrElse(
        'search,
        Array(
          ALNSBuilder.ConfOrder,
          ALNSBuilder.FirstFail,
          ALNSBuilder.LastConf,
          ALNSBuilder.BinSplit,
          ALNSBuilder.ConfOrderValLearn,
          ALNSBuilder.FirstFailValLearn,
          ALNSBuilder.LastConfValLearn,
          ALNSBuilder.BinSplitValLearn
        )
      ).asInstanceOf[Array[String]],

      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String]
    )

    val alns = ALNSSearch(solver, decisionVariables, config)

    alns.search()
  }

  def performBasicSearch(argMap: ArgMap): ALNSSearchResults = {
    val startTime: Long = System.nanoTime()
    val endTime: Long = startTime + (argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L)

    val maximizeObjective: Boolean = solver.objective.objs.head.isMax
    if(!solver.silent) println("Objective type: " + (if(maximizeObjective) "max" else "min"))

    val solsFound = new mutable.ListBuffer[CPIntSol]()

    solver.onSolution{
      val time = System.nanoTime() - startTime
      solsFound += new CPIntSol(decisionVariables.map(_.value), solver.objective.objs.head.best, time, "")
    }

    val stopCondition: (DFSearch) => Boolean = (_: DFSearch) => System.nanoTime() >= endTime

    if(!solver.silent) println("Starting search...")
    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      solver.search(SearchFunctions.conflictOrdering(decisionVariables, maximizeObjective, valLearn = true))
    }

    if(!solver.silent) println("Search done, retrieving results")
    new ALNSSearchResults(solsFound.toArray)
  }

  def parseArgs(map : ArgMap, list: List[String]) : ArgMap = {

    def isSwitch(s : String) = s(0) == '-'

    list match {
      case Nil => map

      case "--timeout" :: value :: tail =>
        parseArgs(map ++ Map('timeout -> value.toLong), tail)

      case "--name" :: value :: tail =>
        parseArgs(map ++ Map('name -> value), tail)

      case "--out" :: value :: tail =>
        parseArgs(map ++ Map('out -> value), tail)

      case "--NOLNS" :: tail =>
        parseArgs(map ++ Map('NOLNS -> true), tail)

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
        while(next.nonEmpty || !isSwitch(next.head)){
          relax += next.head
          next = next.tail
        }
        parseArgs(map ++ Map('relax -> relax), next)

      case "--search" :: value :: tail if !isSwitch(value) =>
        val search = mutable.ListBuffer[String]()
        var next = list.tail
        while(next.nonEmpty || !isSwitch(next.head)){
          search += next.head
          next = next.tail
        }
        parseArgs(map ++ Map('search -> search), next)

      case "--selection" :: value :: tail =>
        parseArgs(map ++ Map('selection -> value), tail)

      case "--metric" :: value :: tail =>
        parseArgs(map ++ Map('metric -> value), tail)

      case option :: tail =>
        if(isSwitch(option) && tail.isEmpty) println("Option " + option + " has no value")
        else println("Unknown option " + option)
        sys.exit(1)
    }
  }

}

