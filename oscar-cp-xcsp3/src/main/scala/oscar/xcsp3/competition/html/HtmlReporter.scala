package oscar.xcsp3.competition.html

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * Utility class to generate an html report from a benchmark directory.
  */
object HtmlReporter extends App{
  if(args.length == 1) generateHtml(args(0))
  else println("Incorrect number of arguments!")

  /**
    * Generates an html report from the given xml object.
    */
  def generateHtml(directory: String): Unit = {
    val (timeout, instances, solvers, configs, allSols, solsByCommit) = scanInstances(directory)

    val objTypes = mutable.Map[String, String]()

    instances.foreach(i => {
      var c = 0
      var objective = "unknown"
      while(objective == "unknown" && c < configs.length){
        if(allSols.contains(i) && allSols(i).contains(configs(c))) {
          val sols = allSols(i)(configs(c))
          if (sols.length > 1) {
            if (sols.last < sols.head) objective = "min"
            else objective = "max"
          }
        }
        c +=1
      }
      objTypes += i -> objective
    })

    val allScores = mutable.Map[String, Int]()
    val scoresByCommits = mutable.Map[String, mutable.Map[String, Int]]()

    instances.foreach(i => {
      val bests = mutable.HashSet[String]()
      val objType = objTypes(i)
      var bestVal = objType match{
        case "min" | "unknown" => Int.MaxValue //If unknown, we assume minimisation
        case "max" => Int.MinValue
      }

      if(allSols.contains(i)) allSols(i).foreach{case (config, sols) =>
          if(sols.nonEmpty){
            val last = sols.last
            if(((objType == "min" || objType == "unknown") && last < bestVal) || (objType == "max" && last > bestVal)){
              bests.clear()
              bests += config
              bestVal = last
            }
            else if(last == bestVal) bests += config
          }
      }

      bests.foreach(config => {
        if(allScores.contains(config)) allScores(config) += 1
        else allScores += config -> 1
      })
    })

    solsByCommit.foreach{case (commit, sols) =>
      val scores = mutable.Map[String, Int]()

      instances.foreach(i => {
        val bests = mutable.HashSet[String]()
        val objType = objTypes(i)
        var bestVal = objType match{
          case "min" | "unknown" => Int.MaxValue //If unknown, we assume minimisation
          case "max" => Int.MinValue
        }

        if(sols.contains(i)) sols(i).foreach{case (solver, solValues) =>
          if(solValues.nonEmpty){
            val last = solValues.last
            if(((objType == "min" || objType == "unknown") && last < bestVal) || (objType == "max" && last > bestVal)){
              bests.clear()
              bests += solver
              bestVal = last
            }
            else if(last == bestVal) bests += solver
          }
        }

        bests.foreach(solver => {
          if(scores.contains(solver)) scores(solver) += 1
          else scores += solver -> 1
        })
      })

      scoresByCommits += commit -> scores
    }

    val htmlWriter = new HtmlWriter("oscar-perf/src/main/scala/oscar/anytime/lns/utils/chart_report_template.html", directory + "/" + "htmlReport.html")
    htmlWriter.addHeading("Benchmark configuration")

    htmlWriter.addParagraph("Time allocated: " + timeout + " seconds")

    htmlWriter.addHeading("Solvers", 2)
    solvers.foreach(solver => {
      htmlWriter.addParagraph(solver)
    })

    htmlWriter.addHeading("Versions", 2)
    solsByCommit.keys.foreach(version => {
      htmlWriter.addParagraph(version)
    })

    htmlWriter.addHeading("Instances", 2)
    instances.foreach(instance => {
      htmlWriter.addParagraph(instance)
    })

    htmlWriter.addHeading("Scores")

    htmlWriter.addHeading("Score per solver on all versions", 2)
    htmlWriter.addElement(
      "table",
      "Score per solver on all versions",
      HtmlWriter.tableToHtmlString(Array(
        configs.map("'" + _ + "'").toArray,
        allScores.toArray.sortBy(_._1).map(_._2.toString)
      ))
    )

    htmlWriter.addHeading("Score per solver per version", 2)
    htmlWriter.addElement(
      "table",
      "Score per solver per version",
      HtmlWriter.tableToHtmlString(renderScores(solvers, scoresByCommits))
    )

    htmlWriter.addHeading("Best solutions")
    solsByCommit.foreach{case (commit, sols) =>
      htmlWriter.addHeading(commit, 2)
      htmlWriter.addElement(
        "table",
        "Best solutions found",
        HtmlWriter.tableToHtmlString(renderBestSols(solvers, instances, sols))
      )
    }

    htmlWriter.writeToHtml()
  }


  //Scans each config_instance file
  def scanInstances(directory: String): (
    Long,
    Seq[String],
    Seq[String],
    Seq[String],
    mutable.Map[String, mutable.Map[String, Seq[Int]]],
    mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[Int]]]]
  ) = {

    val allSols = mutable.Map[String, mutable.Map[String, Seq[Int]]]()
    val solsByCommit = mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[Int]]]]()
    val instances = mutable.HashSet[String]()
    val solvers = mutable.HashSet[String]()
    val configs = mutable.HashSet[String]()
    val dirs = IOUtils.getFolderContent(directory).filter(d => d.isDirectory && !d.getName.startsWith("."))
    var maxTimeout = 0L

    dirs.foreach(dir => {
      val dirName = dir.getName

      val solsByInstances = mutable.Map[String, mutable.Map[String, Seq[Int]]]()

//      println("reading: " + file.getPath)
      val files = IOUtils.getFiles(dir.getAbsolutePath, ".txt")
      files.foreach(file =>{
        val(solver, timeout, instance, sols) = readFile(file)

        if(timeout > maxTimeout) maxTimeout = timeout

        instances += instance
        solvers += solver
        val config = solver + "-" + dirName
        configs += config

        if(solsByInstances.contains(instance)) solsByInstances(instance) += solver -> sols
        else{
          val map = mutable.Map[String, Seq[Int]]()
          map += solver -> sols
          solsByInstances += instance -> map
        }

        if(allSols.contains(instance)) allSols(instance) += config -> sols
        else{
          val map = mutable.Map[String, Seq[Int]]()
          map += config -> sols
          allSols += instance -> map
        }
      })

      solsByCommit += dirName -> solsByInstances
    })

    (maxTimeout, instances.toSeq.sorted, solvers.toSeq.sorted, configs.toSeq.sorted, allSols, solsByCommit)
  }

  def readFile(file: File): (String, Int, String, Seq[Int]) = {

    var solver = ""
    var timeout = 240
    var instance = ""
    val sols = ListBuffer[Int]()

    for (line <- Source.fromFile(file).getLines()) {
      val words = line.split(" ")

      words(0) match{
        case "c" => words(1) match{
          case "instance:" => instance = words(2).stripSuffix(".xml")
          case "solver:" => solver = words(2)
          case "timeout:" => timeout = words(2).toInt
          case _ =>
        }

        case "o" => sols += words(1).toInt

        case _ =>
      }
    }

    (solver, timeout, instance, sols)
  }

  def renderScores(solvers: Seq[String], scoresByCommits: mutable.Map[String, mutable.Map[String, Int]]): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Version'") ++ solvers.map("'" + _ + "'")
    scoresByCommits.foreach{case (commit, scores) =>
      array += Array("'" + commit + "'") ++ solvers.map(solver => scores.getOrElse(solver, 0).toString)
    }
    array.toArray
  }

  def renderBestSols(solvers: Seq[String], instances: Seq[String], sols: mutable.Map[String, mutable.Map[String, Seq[Int]]]): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Instance'") ++ solvers.map("'" + _ + "'")
    instances.foreach(instance => {
      if(sols.contains(instance))
        array += Array("'" + instance + "'") ++ solvers.map(solver => {
          if(sols(instance).contains(solver)){
            val solValues = sols(instance)(solver)
            if(solValues.nonEmpty) solValues.last.toString
            else "null"
          }
          else "null"
        })
      else array += Array("'" + instance + "'") ++ Array.fill[String](solvers.length)("null")
    })
    array.toArray
  }
}
