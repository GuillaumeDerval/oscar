package oscar.xcsp3.competition.html

import java.io.File
import java.text.NumberFormat

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
    val (timeout, instances, solvers, configs, allSols, solsByCommit, statusByCommit) = scanInstances(directory)

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
    val scoresByCommit = mutable.Map[String, mutable.Map[String, Int]]()

    instances.foreach(i => {
      val bests = mutable.HashSet[String]()
      val objType = objTypes(i)
      if(objType != "unknown") {
        var bestVal = objType match {
          case "max" => Int.MinValue
          case "min" => Int.MaxValue
        }

        if (allSols.contains(i)) allSols(i).foreach { case (config, sols) =>
          if (sols.nonEmpty) {
            val last = sols.last
            if ((objType == "min" && last < bestVal) || (objType == "max" && last > bestVal)) {
              bests.clear()
              bests += config
              bestVal = last
            }
            else if (last == bestVal) bests += config
          }
        }

        bests.foreach(config => {
          if (allScores.contains(config)) allScores(config) += 1
          else allScores += config -> 1
        })
      }
    })

    solsByCommit.foreach{case (commit, sols) =>
      val scores = mutable.Map[String, Int]()

      instances.foreach(i => {
        val bests = mutable.HashSet[String]()
        val objType = objTypes(i)
        if(objType != "unknown") {
          var bestVal = objType match {
            case "max" => Int.MinValue
            case "min" => Int.MaxValue
          }

          if (sols.contains(i)) sols(i).foreach { case (solver, solValues) =>
            if (solValues.nonEmpty) {
              val last = solValues.last
              if ((objType == "min" && last < bestVal) || (objType == "max" && last > bestVal)) {
                bests.clear()
                bests += solver
                bestVal = last
              }
              else if (last == bestVal) bests += solver
            }
          }

          bests.foreach(solver => {
            if (scores.contains(solver)) scores(solver) += 1
            else scores += solver -> 1
          })
        }
      })

      scoresByCommit += commit -> scores
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
      HtmlWriter.tableToHtmlString(renderScores(solvers, scoresByCommit))
    )

    htmlWriter.addHeading("Results")
    solsByCommit.foreach{case (commit, sols) =>
      val commitStatus = statusByCommit.getOrElse(commit, mutable.Map[String, mutable.Map[String, String]]())
      htmlWriter.addHeading(commit, 2)
      htmlWriter.addElement(
        "table",
        "Best solutions found",
        HtmlWriter.tableToHtmlString(renderBestSols(solvers, instances, objTypes, sols, commitStatus))
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
    mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[Int]]]],
    mutable.Map[String, mutable.Map[String, mutable.Map[String, String]]]
  ) = {

    val instances = mutable.HashSet[String]()
    val solvers = mutable.HashSet[String]()
    val configs = mutable.HashSet[String]()
    val allSols = mutable.Map[String, mutable.Map[String, Seq[Int]]]()
    val solsByCommit = mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[Int]]]]()
    val statusByCommit = mutable.Map[String, mutable.Map[String, mutable.Map[String, String]]]()

    val dirs = IOUtils.getFolderContent(directory).filter(d => d.isDirectory && !d.getName.startsWith("."))
    var maxTimeout = 0L

    dirs.foreach(dir => {
      val dirName = dir.getName

      val solsByInstance = mutable.Map[String, mutable.Map[String, Seq[Int]]]()
      val statusByInstance = mutable.Map[String, mutable.Map[String, String]]()

//      println("reading: " + file.getPath)
      val files = IOUtils.getFiles(dir.getAbsolutePath, ".txt")
      files.foreach(file =>{
        val(solver, timeout, instance, sols, status) = readFile(file)

        if(timeout > maxTimeout) maxTimeout = timeout

        instances += instance
        solvers += solver
        val config = solver + "-" + dirName
        configs += config

        if(solsByInstance.contains(instance)) solsByInstance(instance) += solver -> sols
        else{
          val map = mutable.Map[String, Seq[Int]]()
          map += solver -> sols
          solsByInstance += instance -> map
        }

        if(statusByInstance.contains(instance)) statusByInstance(instance) += solver -> status
        else{
          val map = mutable.Map[String, String]()
          map += solver -> status
          statusByInstance += instance -> map
        }

        if(allSols.contains(instance)) allSols(instance) += config -> sols
        else{
          val map = mutable.Map[String, Seq[Int]]()
          map += config -> sols
          allSols += instance -> map
        }
      })

      solsByCommit += dirName -> solsByInstance
      statusByCommit += dirName -> statusByInstance
    })

    (maxTimeout, instances.toSeq.sorted, solvers.toSeq.sorted, configs.toSeq.sorted, allSols, solsByCommit, statusByCommit)
  }

  def readFile(file: File): (String, Int, String, Seq[Int], String) = {

    var solver = ""
    var timeout = 240
    var instance = ""
    val sols = ListBuffer[Int]()
    var status = "UNKNOWN"

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

        case "s" => status = words(1)

        case "d" => status = words(1)

        case _ =>
      }

      if(line == "Missing Implementation") status = "UNSUPPORTED"
    }

    (solver, timeout, instance, sols, status)
  }

  def renderScores(solvers: Seq[String], scoresByCommits: mutable.Map[String, mutable.Map[String, Int]]): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Version'") ++ solvers.map("'" + _ + "'")
    scoresByCommits.foreach{case (commit, scores) =>
      array += Array("'" + commit + "'") ++ solvers.map(solver => scores.getOrElse(solver, 0).toString)
    }
    array.toArray
  }

  def renderBestSols(
                      solvers: Seq[String],
                      instances: Seq[String],
                      objTypes: mutable.Map[String, String],
                      sols: mutable.Map[String, mutable.Map[String, Seq[Int]]],
                      status: mutable.Map[String, mutable.Map[String, String]]
                    ): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Instance'", "'Objective'") ++ solvers.map("'" + _ + "'")
    instances.foreach(instance => {
      if(sols.contains(instance))
        array += Array("'" + instance + "'", "'" + objTypes.getOrElse(instance, "unknown") + "'") ++ solvers.map(solver => {
          val solStatus = if(status.contains(instance) && status(instance).contains(solver)) "'" + status(instance)(solver) + "'" else "'UNKNOWN'"
          if(sols(instance).contains(solver)){
            val solValues = sols(instance)(solver)
            if(solValues.nonEmpty){
              val last = NumberFormat.getIntegerInstance().format(solValues.last)
              if(solStatus == "'OPTIMUM'") "'" + last + "*'" else "'" + last + "'"
            }
            else solStatus
          }
          else "'NO_DATA'"
        })
      else array += Array("'" + instance + "'") ++ Array.fill[String](solvers.length)("'NO_DATA'")
    })
    array.toArray
  }
}
