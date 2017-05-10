package oscar.cp.searches.lns

import scala.xml.Elem

class CPIntSol(val values: Array[Int], val objective: Int, val time: Long){

  def asXml: Elem = {
    <solution>
      <time>{time}</time>
      <objective>{objective}</objective>
    </solution>
  }

  override def toString: String = {
    var s = "solution:"
    s += "\n\tobjective: " + objective
    s += "\n\ttime: " + time
    s += "\n\tvalues: [" + values.mkString(", ")
    s += "]"
    s
  }
}
