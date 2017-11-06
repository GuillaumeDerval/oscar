package oscar.anytime.lns.utils

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.xml.{Node, XML}


object HtmlUtils extends App{
  if(args.length == 1) sortBySuccess(args(0))
  else println("Incorrect number of arguments!")

  /**
    * Generates an html report from the given xml object.
    */
  def sortBySuccess(directory: String): Unit = {
    val results: Seq[(String, Seq[(String, Option[Int])])] = scanInstances(directory)

    results.foreach{case (instance, opSols) =>
        val sortedOps = opSols.sortBy(_._2)
        IOUtils.saveToFile(directory + "/" + instance + ".txt", sortedOps.map{case (opName, solVal) => opName + " " + solVal}.mkString("\n"))
    }
  }


  //Scans each config_instance file
  def scanInstances(directory: String): Seq[(String, Seq[(String, Option[Int])])] = {

    val results = mutable.HashMap[String, mutable.ArrayBuffer[(String, Option[Int])]]()
    val files = IOUtils.getFiles(directory, ".xml")

    files.foreach(file => {

//      println("reading: " + file.getPath)
      val content = readXml(XML.loadFile(file))
      if(results.contains(content._1)) results(content._1) += ((content._2, content._3))
      else results += content._1 -> mutable.ArrayBuffer[(String, Option[Int])]((content._2, content._3))
    })

    results.map{case (instance, opSols) => (instance, opSols.toSeq)}.toSeq
  }

  //Reads an xml config_instance file
  def readXml(content: Node): (String, String, Option[Int]) = {
    val opData = (content \\ "operators").head
    val operators = (opData \\ "operator").map(opNode => (opNode \ "name").head.text.trim)

    val sols = (content \\ "solution").map(solNode => (solNode \ "objective").head.text.toInt)

    val bestSol = sols.lastOption
    val config = operators.last
    val instance = (content \ "problem").head.text + "/" + (content \ "instance").head.text

    (instance, config, bestSol)
  }
}
