package oscar.anytime.lns.utils

import java.io.{File, FileWriter}

import scala.xml.{Elem, NodeBuffer, PrettyPrinter}


object XmlWriter{

  private def resultsToXml(bestKnown:Option[Int], maxObjective:Boolean, solutions: Iterable[(Long, Int)]): Elem = {

    val xml = new NodeBuffer()

    xml += <objective_type>{if(maxObjective) "max" else "min"}</objective_type>
    if(bestKnown.isDefined) xml += <best_known_objective>{bestKnown.get}</best_known_objective>

    xml += (<solutions>{solutions.map{
      case (time: Long, objective: Int) =>
        <solution>
          <time>{time}</time>
          <objective>{objective}</objective>
        </solution>
    }}</solutions>)

    <results>{xml}</results>
  }

  /**
    * writes the specified elements into an xml file.
    * @param directory the path of a directory where the file will be created
    * @param method the name of the method which is the first part of the filename
    * @param instance the name of the instance which is the second part of the filename
    * @param bestKnown the objective value of the best known solution if known.
    * @param maxObjective whether the objective is to maximise (true) or minimise (false).
    * @param solutions an iterable of pairs Time (Long) - solution objective (Int).
    * @return an xml node.
    */
  def writeToXml(directory:String, method: String, instance:String, bestKnown:Option[Int] = None, maxObjective:Boolean, solutions: Iterable[(Long, Int)]): Unit = {
    val xml = resultsToXml(bestKnown, maxObjective, solutions)

    val output = new File(directory + "/" + method + "_" + instance + ".xml")
    if(!output.exists()){
      output.getParentFile.mkdirs()
      output.createNewFile()
    }

    val writer = new FileWriter(output, true)
    writer.write(new PrettyPrinter(80, 4).format(xml))
    writer.close()
  }
}