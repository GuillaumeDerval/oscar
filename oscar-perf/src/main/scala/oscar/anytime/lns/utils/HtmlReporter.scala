package oscar.anytime.lns.utils

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.xml.{Node, XML}

/**
  * Utility class to generate an html report from a benchmark directory.
  */
//TODO: add search information
object HtmlReporter extends App{
  if(args.length == 1) generateHtml(args(0))
  else println("Incorrect number of arguments!")

  /**
    * Generates an html report from the given xml object.
    */
  def generateHtml(directory: String): Unit = {
    val (timeout, instances, configs, instanceTypes, bestknownSolutions, data) = scanInstances(directory)
    val nBks = bestknownSolutions.values.count(_.isDefined)
    val (bestSols, anyTimeScores, anyTimeGaps, anyTimeQuality, instanceStats) = processStats(instances, configs, data, nBks)

    val htmlWriter = new HtmlWriter("oscar-perf/src/main/scala/oscar/anytime/lns/utils/chart_report_template.html", directory + "/" + IOUtils.getFileName(directory) + "_htmlReport.html")
    htmlWriter.addHeading("Benchmark configuration")

    htmlWriter.addParagraph("Time allocated: " + timeout/1000000000.0 + " seconds")

    htmlWriter.addHeading("Search configurations", 2)
    configs.foreach(config => {
      htmlWriter.addParagraph(config)
    })

    htmlWriter.addHeading("General statistics")

    htmlWriter.addHeading("Best solution found per method", 2)
    htmlWriter.addElement(
      "table",
      "Best solution found",
      HtmlWriter.tableToHtmlString(renderBestSols(bestSols, configs, instances, instanceTypes.toMap, bestknownSolutions.toMap))
    )

    htmlWriter.addHeading("Score", 3)
    htmlWriter.addElement(
      "line",
      "Anytime score",
      HtmlWriter.tableToHtmlString(renderScoresByTime(anyTimeScores, configs, timeout, stepped = true))
    )

    val anyTimeGapsArray = renderGapsByTime(anyTimeGaps, configs, timeout, stepped = true)
    if(anyTimeGapsArray.length > 1)
      htmlWriter.addHeading("Distance to BKS", 3)
      htmlWriter.addElement(
        "line",
        "Anytime relative distance to BKS",
        HtmlWriter.tableToHtmlString(anyTimeGapsArray)
      )

    htmlWriter.addHeading("Quality", 3)
    htmlWriter.addElement(
      "line",
      "Anytime quality",
      HtmlWriter.tableToHtmlString(renderQualityByTime(anyTimeQuality, configs, timeout, stepped = true))
    )

    htmlWriter.addHeading("Instances statistics")

    instanceStats.sortBy(_._1).foreach{case(name, sols, scores, gaps, opScores) =>
      htmlWriter.addHeading(name, 2)

      htmlWriter.addElement(
        "line",
        "Search evolution",
        HtmlWriter.tableToHtmlString(renderSolsByTime(sols, configs, timeout, stepped = true))
      )

      opScores.foreach{case (config, operators, opWeights) =>
        if(config != "Baseline-best" && config != "Baseline-worst" && config != "Baseline")
          htmlWriter.addHeading("Anytime " + config + "'s operators scores", 3)
          htmlWriter.addElement(
            "line",
            "Operator weights evolution for " + config,
            HtmlWriter.tableToHtmlString(renderOpScoresByTime(opWeights, operators, stepped = true))
          )
      }

//      htmlWriter.addElement(
//        "line",
//        "Score evolution",
//        HtmlWriter.tableToHtmlString(renderScoresByTime(scores, configs, timeout, stepped = true))
//      )

//      if(gaps.isDefined)
//        htmlWriter.addElement(
//          "line",
//          "Gap evolution",
//          HtmlWriter.tableToHtmlString(renderGapsByTime(gaps.get, configs, timeout, stepped = true))
//        )
    }

    htmlWriter.writeToHtml()
  }


  //Scans each config_instance file
  def scanInstances(directory: String): (
    Long,
    Seq[String],
    Seq[String],
    mutable.Map[String, String],
    mutable.Map[String, Option[Int]],
    mutable.Map[String, (Boolean, Option[Int], ArrayBuffer[(Long, String, Int)], mutable.Map[String, Seq[String]], mutable.Map[String, Seq[(Long, String, Double)]])]
  ) = {

    val data = mutable.Map[String, (Boolean, Option[Int], ArrayBuffer[(Long, String, Int)], mutable.Map[String, Seq[String]], mutable.Map[String, Seq[(Long, String, Double)]])]()
    val instances = mutable.HashSet[String]()
    val instanceTypes = mutable.Map[String, String]()
    val bks = mutable.Map[String, Option[Int]]()
    val configs = mutable.HashSet[String]()
    val files = IOUtils.getFiles(directory, ".xml")
    var maxTimeout = 0L

    for(file <- files){

//      println("reading: " + file.getPath)
      val(config, timeout, instance, problem, isMax, bestKnown, sols, operators, opWeights) = readXml(XML.loadFile(file))

      if(timeout > maxTimeout) maxTimeout = timeout

      instances.add(instance)
      if(!instanceTypes.contains(instance)) instanceTypes += instance -> problem
      if(!bks.contains(instance)) bks += instance -> bestKnown
      configs.add(config)

      if(data.contains(instance)){
        data(instance)._3 ++= sols
        data(instance)._4 += config -> operators
        data(instance)._5 += config -> opWeights
      }
      else{
        val opMap = mutable.Map[String, Seq[String]]()
        opMap += config -> operators
        val opScoresMap = mutable.Map[String, Seq[(Long, String, Double)]]()
        opScoresMap += config -> opWeights
        data += instance -> (isMax, bestKnown, sols.to[ArrayBuffer], opMap, opScoresMap)
      }
      Unit //Workaround for strange bug in 2.12
    }

    (maxTimeout, instances.toSeq.sorted, configs.toSeq.sorted, instanceTypes, bks, data)
  }

  //Reads an xml config_instance file
  def readXml(content: Node): (String, Long, String, String, Boolean, Option[Int], Seq[(Long, String, Int)], Seq[String], Seq[(Long, String, Double)]) = {
    val config = (content \ "config").head.text
    val timeout = (content \ "timeout").head.text.toLong
    val instance = (content \ "instance").head.text
    val problem = (content \ "problem").head.text

    val isMax = (content \ "objective_type").head.text match{
      case "max" => true
      case "min" => false
    }

    val bksNode = content \ "best_known_objective"
    val bestKnown = if(bksNode.isEmpty) None else Some(bksNode.head.text.toInt)

    val sols = (content \\ "solution").map(solNode =>(
      (solNode \ "time").head.text.toLong,
      config,
      (solNode \ "objective").head.text.toInt
    ))

    val opData = (content \\ "operators").head
    val operators = (opData \\ "operator").map(opNode => (opNode \ "name").head.text.trim)

    val opWeights = (content \\ "score_update").map(scoreNode =>(
      (scoreNode \ "time").head.text.toLong,
      (scoreNode \ "operator").head.text.trim,
      (scoreNode \ "score").head.text.toDouble
    ))

    (config, timeout, instance, problem, isMax, bestKnown, sols, operators, opWeights)
  }


  //Aggregates sols by time
  def solsByTime(configs: Seq[String], sols: Seq[(Long, String, Int)]): Seq[(Long, Array[Option[Int]])] = {
    val mapping = configs.zipWithIndex.toMap

    val currentSols: Array[Option[Int]] = Array.fill(configs.length)(None)
    val solsByTime = mutable.ArrayBuffer[(Long, Array[Option[Int]])]()

    sols.sortBy(_._1)
      .map{case (time, config, objective) => ((time/100000000.0).ceil.toLong, config, objective)}
      .foreach{case (time, config, objective) =>
        if(currentSols(mapping(config)).isEmpty || objective < currentSols(mapping(config)).get) {
          currentSols(mapping(config)) = Some(objective)
          if(solsByTime.nonEmpty && solsByTime.last._1 == time) solsByTime.last._2(mapping(config)) = Some(objective)
          else solsByTime += ((time, currentSols.clone()))
        }
      }

    solsByTime
  }

  //Aggregates operator scores by time
  def opScoresByTime(operators: Seq[String], scores: Seq[(Long, String, Double)]): Seq[(Long, Array[Option[Double]])] = {
    val mapping = operators.zipWithIndex.toMap

    val currentScores: Array[Option[Double]] = Array.fill(operators.length)(None)
    val scoresByTime = mutable.ArrayBuffer[(Long, Array[Option[Double]])]()

    scores.filter(_._2 != "dummy").sortBy(_._1)
      .map{case (time, operator, score) => ((time/100000000.0).ceil.toLong, operator, score)}
      .foreach{case (time, operator, score) =>
        currentScores(mapping(operator)) = Some(score)
        if(scoresByTime.nonEmpty && scoresByTime.last._1 == time) scoresByTime.last._2(mapping(operator)) = Some(score)
        else scoresByTime += ((time, currentScores.clone()))
      }

    scoresByTime
  }

  //Aggreagates gaps by time
  //TODO: make generic method for this and scores by time
  def gapsByTime(
                  instances: Seq[String],
                  configs: Seq[String],
                  gaps: ArrayBuffer[(Long, String, Array[Option[Double]])]
                ): ArrayBuffer[(Long, Array[Array[Option[Double]]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentGaps: Array[Array[Option[Double]]] = Array.fill(instances.length, configs.length)(None)
    val gapsByTime = mutable.ArrayBuffer[(Long, Array[Array[Option[Double]]])]()

    gaps.sortBy(_._1).foreach{case (time, instance, gapValues) =>
      val instanceIndex = instMapping(instance)
      gapValues.indices.foreach(configIndex =>{
        currentGaps(instanceIndex)(configIndex) = gapValues(configIndex)
      })
      if(gapsByTime.nonEmpty && gapsByTime.last._1 == time) gapsByTime.last._2(instanceIndex) = currentGaps(instanceIndex).clone
      else gapsByTime += ((time, arrayCopy(currentGaps)))
    }

    gapsByTime
  }

  //Aggregates scores by time
  def scoresByTime(
                    instances: Seq[String],
                    configs: Seq[String],
                    scores: ArrayBuffer[(Long, String, Array[Int])]
                  ): ArrayBuffer[(Long, Array[Array[Int]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentScores: Array[Array[Int]] = Array.fill(instances.length, configs.length)(0)
    val scoresByTime = mutable.ArrayBuffer[(Long, Array[Array[Int]])]()

    scores.sortBy(_._1).foreach{case (time, instance, scoreValues) =>
      val instanceIndex = instMapping(instance)
      scoreValues.indices.foreach(configIndex =>{
        currentScores(instanceIndex)(configIndex) = scoreValues(configIndex)
      })
      if(scoresByTime.nonEmpty && scoresByTime.last._1 == time) scoresByTime.last._2(instanceIndex) = currentScores(instanceIndex).clone()
      else scoresByTime += ((time, arrayCopy(currentScores)))
    }

    scoresByTime
  }

  def qualityByTime(
                     instances: Seq[String],
                     configs: Seq[String],
                     quality: ArrayBuffer[(Long, String, Array[Option[Double]])]
                   ): ArrayBuffer[(Long, Array[Array[Option[Double]]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentQuality: Array[Array[Option[Double]]] = Array.fill(instances.length, configs.length)(None)
    val qualityByTime = mutable.ArrayBuffer[(Long, Array[Array[Option[Double]]])]()

    quality.sortBy(_._1).foreach{case (time, instance, qualityValues) =>
      val instanceIndex = instMapping(instance)
      qualityValues.indices.foreach(configIndex =>{
        currentQuality(instanceIndex)(configIndex) = qualityValues(configIndex)
      })
      if(qualityByTime.nonEmpty && qualityByTime.last._1 == time) qualityByTime.last._2(instanceIndex) = currentQuality(instanceIndex).clone
      else qualityByTime += ((time, arrayCopy(currentQuality)))
    }

    qualityByTime
  }

  //Process statistics
  def processStats(
                    instances: Seq[String],
                    configs: Seq[String],
                    data: mutable.Map[String, (Boolean, Option[Int], ArrayBuffer[(Long, String, Int)], mutable.Map[String, Seq[String]], mutable.Map[String, Seq[(Long, String, Double)]])],
                    nBks: Int
                  ): (
    Array[Array[Option[Int]]],
    ArrayBuffer[(Long, Array[Int])],
    ArrayBuffer[(Long, Array[Option[Double]])],
    ArrayBuffer[(Long, Array[Double])],
    ArrayBuffer[(String, Seq[(Long, Array[Option[Int]])], Seq[(Long, Array[Int])], Option[Seq[(Long, Array[Option[Double]])]], Array[(String, Seq[String], Seq[(Long, Array[Option[Double]])])])]
  ) = {

    val nBests = 3 //Number of best configs to reward (the reward is proportional to the place in the ranking)

    val instMapping = instances.zipWithIndex.toMap //Mapping to the index of each instance
    val confMapping = configs.zipWithIndex.toMap //Mapping to the index of each config

    val scores = mutable.ArrayBuffer[(Long, String, Array[Int])]()
    val gaps = mutable.ArrayBuffer[(Long, String, Array[Option[Double]])]()
    val quality = mutable.ArrayBuffer[(Long, String, Array[Option[Double]])]()
    val bestSols = Array.fill[Option[Int]](instances.length, configs.length)(None)

    // Instance stats: (name, sols, gaps, scores, opScores)
    val instanceStats = mutable.ArrayBuffer[(
      String,
      Seq[(Long, Array[Option[Int]])],
      Seq[(Long, Array[Int])],
      Option[Seq[(Long, Array[Option[Double]])]],
      Array[(String, Seq[String], Seq[(Long, Array[Option[Double]])])]
    )]()

    //Scanning each instance data:
    data.foreach(instanceData => {
      val (name, content) = instanceData
      val (isMax, bestKnown, solsFound, operators, opScores) = content
      val sortedSols = solsByTime(configs, solsFound)
      val startSol: Int = if(sortedSols.isEmpty) Int.MaxValue else sortedSols.head._2.map(_.getOrElse(Int.MaxValue)).min

      //Computing best sols:
      if(sortedSols.nonEmpty){
        val bests = sortedSols.last._2
        val instanceIndex = instMapping(name)
        configs.indices.foreach(configIndex => bestSols(instanceIndex)(configIndex) = bests(configIndex))
      }

      //Computing gaps:
      val instanceGaps: Option[Seq[(Long, Array[Option[Double]])]] = if(bestKnown.isDefined)
        Some(sortedSols.map {case (time, sols) =>
          (time, sols.map {
            case None => None
            case objective: Option[Int] => {
              val diff = objective.get - bestKnown.get //TODO take into account objective type
              Some(diff.toDouble / (startSol - bestKnown.get))
            }
          })
        })
      else None

      //Adding gaps:
      if(instanceGaps.isDefined) instanceGaps.get.foreach{case (time, gapValues) => gaps += ((time, name, gapValues))}


      val instanceScores = ListBuffer[(Long, Array[Int])]()
      sortedSols.foreach{case (time, sols) =>
        //Computing scores:
        val objectives = mutable.Map[Int, mutable.Set[String]]()

        //associating each config to its objective rank:
        sols.indices.foreach(i => if(sols(i).isDefined){
          val objective = sols(i).get
          if(!objectives.contains(objective)) objectives += objective -> mutable.HashSet[String]()
          objectives(objective) += configs(i)
        })

        //Sorting ranks accordingly:
        val rank = (
          if(isMax) objectives.toSeq.sortWith(_._1 > _._1)
          else objectives.toSeq.sortWith(_._1 < _._1)
        ).map(_._2)

        //Computing scores based on best ranks:
        val score = Array.fill[Int](configs.length)(0)
        var i = 0
        while(i < nBests && i < rank.length){
          rank(i).foreach(config => score(confMapping(config)) = nBests - i)
          i+=1
        }

        instanceScores += ((time, score))
        scores += ((time, name, score))

        //Computing quality:
        val bestSol: Option[Int] = sols.min
        if(bestSol.isDefined){
          val solQuality = sols.map(sol => {
            if(sol.isDefined) Some(1 - (sol.get - bestSol.get).toDouble/(startSol - bestSol.get))
            else None
          })
          quality += ((time, name, solQuality))
        }
      }

      //Computing operator scores:
      val opScoresByConfig = mutable.ArrayBuffer[(String, Seq[String], Seq[(Long, Array[Option[Double]])])]()
      operators.foreach{case (config, ops) =>
        if(opScores.contains(config)) opScoresByConfig += ((config, ops, opScoresByTime(ops, opScores(config))))
      }

      instanceStats += ((name, sortedSols, instanceScores, instanceGaps, opScoresByConfig.toArray))
    })

    //Computing any time gaps:
    val anyTimeGap = gapsByTime(instances, configs, gaps).sortBy(_._1).map{case (time, gapOptions) =>
      (time, configs.indices.map(i =>{
        val gapValues = gapOptions.map(_(i)).filter(_.isDefined).map(_.get)
//        if(gapValues.length == nBks)
          Some(gapValues.sum/gapValues.length).asInstanceOf[Option[Double]]
//        else None
      }).toArray)
    }

    //Computing any time scores:
    val anyTimeScore = scoresByTime(instances, configs, scores).sortBy(_._1).map{case (time, scoreValues) =>
      (time, configs.indices.map(i => {scoreValues.map(_(i)).sum}).toArray)
    }

    //Computing any time quality:
    val anyTimeQuality = qualityByTime(instances, configs, quality).map{case (time, qualityValues) =>
      (time, configs.indices.map(i => {
        val configQuality = qualityValues.map(_(i)).filter(_.isDefined).map(_.get)
        configQuality.sum/configQuality.length
      }).toArray)
    }

    (bestSols, anyTimeScore, anyTimeGap, anyTimeQuality, instanceStats)
  }

  def renderBestSols(
                      bestSols: Array[Array[Option[Int]]],
                      configs: Seq[String],
                      instances: Seq[String],
                      instanceTypes: Map[String, String],
                      bestKnownSolutions: Map[String, Option[Int]]
                    ): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Instance'", "'Set'", "'Best known solution'") ++ configs.map("'" + _ + "'")
    bestSols.indices.foreach(i => {
      array += Array("'" + instances(i) + "'", "'" + instanceTypes(instances(i)) + "'", bestKnownSolutions(instances(i)) match{
        case None => "null"
        case Some(bks: Int) => bks.toString
      }) ++ bestSols(i).map{
        case None => "null"
        case Some(bestSol: Int) => bestSol.toString
      }
    })
    array.toArray
  }

  def renderScoresByTime(scores: Seq[(Long, Array[Int])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    scores.foreach{case (time, scoreValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = scoreValues.map(_.toString)
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderGapsByTime(gaps: Seq[(Long, Array[Option[Double]])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    gaps.foreach{case (time, gapValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = gapValues.map{
        case None => "null"
        case Some(gap: Double) => gap.toString
      }
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderSolsByTime(sols: Seq[(Long, Array[Option[Int]])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] ={
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    sols.foreach{case (time, solValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = solValues.map{
        case None => "null"
        case Some(sol: Int) => sol.toString
      }
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderQualityByTime(quality: Seq[(Long, Array[Double])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    quality.foreach{case (time, qualityValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = qualityValues.map(_.toString)
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderOpScoresByTime(opScores: Seq[(Long, Array[Option[Double]])], operators: Seq[String], stepped: Boolean = false): Array[Array[String]] ={
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ operators.map("'" + _ + "'")
    opScores.foreach{case (time, scoreValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = scoreValues.map{
        case None => "null"
        case Some(score: Double) => score.toString
      }
      array += t ++ previous
    }}
    array.toArray
  }

  private def arrayCopy[T](array: Array[Array[T]])(implicit e: ClassTag[T]): Array[Array[T]] = array.map(_.clone)
}
