package oscar.anytime.lns.models

import oscar.anytime.lns.utils.{IOUtils, XmlWriter}
import oscar.cp.searches.lns.search.ALNSConfig
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.searches.alns.operators.ALNSBuilder
import oscar.modeling.solvers.cp.searches.alns.search.ALNSSearch
import oscar.xcsp3.XCSP3Parser2

import scala.collection.mutable

class XCSP(file: String){
  val md = new ModelDeclaration

  def main(args: Array[String]): Unit = {

    val argMap = parseArgs(Map(), args.toList)
    if(argMap.nonEmpty && !argMap.contains('name))
      println("WARNING: A config name should be provided if using custom parameters!")

    //Parsing the instance and instantiating model declaration
    val (vars, solutionGenerator) = XCSP3Parser2.parse(md, file)

    val config = new ALNSConfig(
      timeout = argMap.getOrElse('timeout, 0L).asInstanceOf[Long] * 1000000000L,
      coupled = argMap.getOrElse('coupled, true).asInstanceOf[Boolean],
      learning = argMap.getOrElse('learning, false).asInstanceOf[Boolean],

      argMap.getOrElse(
        'relax,
        Array(ALNSBuilder.Random, ALNSBuilder.KSuccessive, ALNSBuilder.PropGuided, ALNSBuilder.RevPropGuided)
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

    val alns = ALNSSearch(
      md,
      vars,
      config,
      solutionGenerator
    )

    val result = alns.search()

    XmlWriter.writeToXml(
      argMap.getOrElse('out, "../ALNS-bench-results/").asInstanceOf[String],
      argMap.getOrElse('name, "default").asInstanceOf[String],
      IOUtils.getFileName(file),
      IOUtils.getParentName(file),
      Int.MaxValue,
      maxObjective = false,
      result.solutions
    )
  }

  type ArgMap = Map[Symbol, Any]

  def parseArgs(map : ArgMap, list: List[String]) : ArgMap = {
    def isSwitch(s : String) = s(0) == '-'
    list match {
      case Nil => map

      case "-t" :: value :: tail =>
        parseArgs(map ++ Map('timeout -> value.toLong), tail)

      case "--timeout" :: value :: tail =>
        parseArgs(map ++ Map('timeout -> value.toLong), tail)

      case "--name" :: value :: tail =>
        parseArgs(map ++ Map('name -> value), tail)

      case "--coupled" :: value :: tail =>
        parseArgs(map ++ Map('coupled -> value.toBoolean), tail)

      case "--learning" :: value :: tail =>
        parseArgs(map ++ Map('learning -> value.toBoolean), tail)

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
