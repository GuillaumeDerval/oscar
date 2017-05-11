package oscar.anytime.lns.utils


import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.xml.{Node, XML}

/**
  * Utility class to generate an html report from a benchmark directory.
  */
object HtmlReporter{

  /**
    * Generates an html report from the given xml object.
    */
  def generateHtml(directory: String, output: String = ""): Unit = {

//    val htmlWriter = new HtmlWriter("resources/chart_report_template.html", directory + "htmlReport.html")
//    htmlWriter.addHeading("General statistics")
//
//    htmlWriter.addParagraph("Best solution found per method")
//    htmlWriter.addElement(
//      "table",
//      "Best solution found",
//      HtmlWriter.tableToHtmlString(scanBestSols(results, configKeys))
//    )
//
//    htmlWriter.addElement(
//      "line",
//      "Number of best solutions found per method over time",
//      HtmlWriter.tableToHtmlString(scanAnyTimeBest(results, configKeys, timeout, makeStep = true))
//    )
//
//    htmlWriter.addHeading("Instances statistics")
//
//    //Scanning each instance node:
//    (results \ "instance").foreach(instanceNode => {
//      htmlWriter.addParagraph((instanceNode \ "name").head.text)
//
//      htmlWriter.addElement(
//        "line",
//        "Search evolution",
//        HtmlWriter.tableToHtmlString(scanSearchSols(instanceNode, configKeys, timeout, makeStep = true))
//      )
//    })
//
//    htmlWriter.writeToHtml()
  }


  //Scans each config_instance file
  def scanInstances(directory: String): (
    Seq[String],
    Seq[String],
    mutable.Map[String, String],
    mutable.Map[String, (Boolean, Option[Int], ListBuffer[(Long, String, Int)])]
  ) = {

    val data = mutable.Map[String, (Boolean, Option[Int], ListBuffer[(Long, String, Int)])]()
    val instances = mutable.HashSet[String]()
    val instanceTypes = mutable.Map[String, String]()
    val configs = mutable.HashSet[String]()
    val files = IOUtils.getFolderContent(directory)

    files.foreach(file => if(file.getName.endsWith(".xml")){

      val(config, instance, problem, isMax, bestKnown, sols) = readXml(XML.loadFile(file))

      instances.add(instance)
      if(!instanceTypes.contains(instance)) instanceTypes += instance -> problem
      configs.add(config)

      if(data.contains(instance)) data(instance)._3 ++= sols
      else data += instance -> (isMax, bestKnown, sols.to[ListBuffer])
    })

    (instances.toSeq.sorted, configs.toSeq.sorted, instanceTypes, data)
  }

  //Reads an xml config_instance file
  def readXml(content: Node): (String, String, String, Boolean, Option[Int], Seq[(Long, String, Int)]) = {
    val config = (content \ "config").head.text
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

    (config, instance, problem, isMax, bestKnown, sols)
  }


  //Aggregates sols by time
  def solsByTime(configs: Seq[String], sols: ArrayBuffer[(Long, String, Int)]): Seq[(Long, Array[Option[Int]])] = {
    val mapping = configs.zipWithIndex.toMap

    val currentSols: Array[Option[Int]] = Array.fill(configs.length)(None)
    val solsByTime = mutable.ArrayBuffer[(Long, Array[Option[Int]])]()

    sols.sortBy(_._1).foreach{case (time, config, objective) =>
      if(currentSols(mapping(config)).isEmpty || objective < currentSols(mapping(config)).get) {
        currentSols(mapping(config)) = Some(objective)
        if(solsByTime.nonEmpty && solsByTime.last._1 == time) solsByTime.last._2(mapping(config)) = Some(objective)
        else solsByTime += ((time, currentSols.clone()))
      }
    }

    solsByTime
  }

  //Aggreagates gaps by time
  //TODO: make generic method for this and scores by time
  def gapsByTime(instances: Seq[String], configs: Seq[String], gaps: ArrayBuffer[(Long, String, Array[Option[Double]])]): ArrayBuffer[(Long, Array[Array[Option[Double]]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentGaps: Array[Array[Option[Double]]] = Array.fill(instances.length, configs.length)(None)
    val gapsByTime = mutable.ArrayBuffer[(Long, Array[Array[Option[Double]]])]()

    gaps.sortBy(_._1).foreach{case (time, instance, gapValues) =>
      val instanceIndex = instMapping(instance)
      gapValues.indices.foreach(configIndex =>{
        currentGaps(instanceIndex)(configIndex) = gapValues(configIndex)
      })
      if(gapsByTime.nonEmpty && gapsByTime.last._1 == time) gapsByTime.last._2(instanceIndex) = currentGaps(instanceIndex)
      else gapsByTime += ((time, currentGaps.clone()))
    }

    gapsByTime
  }

  //Aggregates scores by time
  def scoresByTime(instances: Seq[String], configs: Seq[String], scores: ArrayBuffer[(Long, String, Array[Int])]): ArrayBuffer[(Long, Array[Array[Int]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentScores: Array[Array[Int]] = Array.fill(instances.length, configs.length)(0)
    val scoresByTime = mutable.ArrayBuffer[(Long, Array[Array[Int]])]()

    scores.sortBy(_._1).foreach{case (time, instance, gapValues) =>
      val instanceIndex = instMapping(instance)
      gapValues.indices.foreach(configIndex =>{
        currentScores(instanceIndex)(configIndex) = gapValues(configIndex)
      })
      if(scoresByTime.nonEmpty && scoresByTime.last._1 == time) scoresByTime.last._2(instanceIndex) = currentScores(instanceIndex)
      else scoresByTime += ((time, currentScores.clone()))
    }

    scoresByTime
  }

  //Process statistics
  def processGlobalStats(instances: Seq[String], configs: Seq[String], data: mutable.Map[String, (Boolean, Option[Int], ArrayBuffer[(Long, String, Int)])]) = {

    val nBests = 3 //Number of best configs to reward (the reward is proportional to the place in the ranking)

    val instMapping = instances.zipWithIndex.toMap //Mapping to the index of each instance
    val confMapping = configs.zipWithIndex.toMap //Mapping to the index of each config

    val scores = mutable.ArrayBuffer[(Long, String, Array[Int])]()
    val gaps = mutable.ArrayBuffer[(Long, String, Array[Option[Double]])]()
    val bestSols = Array.fill[Option[Int]](configs.length, instances.length)(None)
//    val instanceStats = ArrayBuffer[()]() // Instance stats: (name, sols, gaps, scores)


    //Scanning each instance data:
    data.foreach(instanceData => {
      val (name, content) = instanceData
      val (isMax, bestKnown, solsFound) = content
      val sortedSols = solsByTime(configs, solsFound)

      //Computing best sols:
      val bests = sortedSols.last._2
      val instanceIndex = instMapping(name)
      configs.indices.foreach(configIndex => bestSols(configIndex)(instanceIndex) = bests(configIndex))

      //Computing gaps:
      val instanceGaps: Option[Seq[(Long, Array[Option[Double]])]] = if(bestKnown.isDefined)
        Some(sortedSols.map {case (time, sols) =>
          (time, sols.map {
            case None => None
            case objective: Option[Int] => Some(Math.abs(bestKnown.get - objective.get).toDouble / bestKnown.get.toDouble)
          })
        })
      else None

//      //Adding gaps:
//      if(instanceGaps.isDefined) instanceGaps.foreach{case (time, gapValues) => }

      //Computing scores:
      sortedSols.foreach{case (time, sols) =>
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

        scores += ((time, name, score))
      }

//      instanceStats +=(name, sortedSols)
    })

    //Computing any time gaps:
    val anyTimeGap = gapsByTime(instances, configs, gaps).sortBy(_._1).map{case (time, gapOptions) =>
      (time, configs.indices.map(i =>{
        val gapValues = gapOptions.map(_(i)).filter(_.isDefined).map(_.get)
        if(gapValues.length == gapOptions.length)
          Some(Math.pow(gapValues.foldLeft(1.0) { _ * _ }, 1.0/gapValues.length))
        else None
      }))
    }

    //Computing any time scores:
    val anyTimeScore = scoresByTime(instances, configs, scores).sortBy(_._1).map{case (time, scoreValues) =>
      (time, configs.indices.map(i =>{scoreValues.map(_(i)).sum}))
    }

  }
}
