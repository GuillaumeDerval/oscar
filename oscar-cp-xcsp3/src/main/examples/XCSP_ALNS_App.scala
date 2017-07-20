
import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch, ALNSSearchResults}
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.xcsp3.XCSP3Parser2

import scala.collection.mutable
import scala.util.Random

object XCSP_ALNS_App extends App{

  type ArgMap = Map[Symbol, Any]

  val argMap = parseArgs(Map(), args.toList)
  val instance = argMap.getOrElse('instance, "unknown").asInstanceOf[String]
  if(instance == "unknown"){
    println("No instance has been provided!")
    sys.exit(1)
  }

  val md = new ModelDeclaration

  //Parsing the instance and instantiating model declaration
  val (vars, solutionGenerator) = XCSP3Parser2.parse(md, instance)

  val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
  md.setCurrentModel(model)

  val decisionVariables: Array[CPIntVar] = vars.map(model.getRepresentative(_).realCPVar)

  val solver: CPSolver = model.cpSolver

  //    println("args:\n" + argMap.mkString("\n") + "\n")

  val result = performALNS(argMap)

  println("solutions found: ")
  result.solutions.foreach(println)

  def performALNS(argMap: ArgMap): ALNSSearchResults = {
    val seed = Random.nextInt()
    println("Seed: " + seed)
    Random.setSeed(seed)
    val config = new ALNSConfig(
      argMap.getOrElse('timeout, 300L).asInstanceOf[Long] * 1000000000L,
      1000,
      coupled = argMap.getOrElse('coupled, false).asInstanceOf[Boolean],
      learning = argMap.getOrElse('learning, false).asInstanceOf[Boolean],

      argMap.getOrElse(
        'relax,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided)
      ).asInstanceOf[Array[String]],

      argMap.getOrElse(
        'search,
        Array( ALNSBuilder.ConfOrder, ALNSBuilder.FirstFail, ALNSBuilder.LastConf, ALNSBuilder.BinSplit)
      ).asInstanceOf[Array[String]],

      argMap.getOrElse('valLearn, true).asInstanceOf[Boolean],

      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('selection, ALNSBuilder.RWheel).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String],
      argMap.getOrElse('metric, ALNSBuilder.AvgImprov).asInstanceOf[String]
    )

    val alns = ALNSSearch(solver, decisionVariables, config)

    alns.search()
  }

  def parseArgs(map : ArgMap, list: List[String]) : ArgMap = {

    def isSwitch(s : String) = s(0) == '-'

    list match {
      case Nil => map

      case "--timeout" :: value :: tail =>
        parseArgs(map ++ Map('timeout -> value.toLong), tail)

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

      case "--val-heuristic" :: value :: tail =>
        parseArgs(map ++ Map('valHeuristic -> value), tail)

      case "--val-learn" :: tail => tail match{
        case value :: remTail =>
          if(isSwitch(value)) parseArgs(map ++ Map('valLearn -> true), tail)
          else parseArgs(map ++ Map('valLearn -> value.toBoolean), remTail)
        case Nil => map ++ Map('valLearn -> true)
      }

      case "--selection" :: value :: tail =>
        parseArgs(map ++ Map('selection -> value), tail)

      case "--metric" :: value :: tail =>
        parseArgs(map ++ Map('metric -> value), tail)

      case value :: tail =>
        if(!isSwitch(value)) parseArgs(map ++ Map('instance -> value), tail)
        else {
          println("Unknown option " + value)
          sys.exit(1)
        }

      case option :: tail =>
        if(isSwitch(option) && tail.isEmpty) println("Option " + option + " has no value")
        else println("Unknown option " + option)
        sys.exit(1)
    }
  }
}
