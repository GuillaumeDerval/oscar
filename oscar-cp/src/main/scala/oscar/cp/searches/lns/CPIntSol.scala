package oscar.cp.searches.lns

import oscar.cp.CPIntVar

import scala.xml.Elem

object CPIntSol{
  def getXCSPInstantiation(vars: Iterable[CPIntVar]): String = {
    val str1 = "<instantiation>\n\t<list>\n\t\t"
    val elems = vars.map(x => x.name).mkString(" ")
    var str2 = "\n\t</list>\n\t<values>\n\t\t"
    val vals = vars.map(x => x.value.toString).mkString(" ")
    val str3 = "\n\t</values>\n</instantiation>"
    str1 + elems + str2 + vals + str3
  }
}

class CPIntSol(val values: Array[Int], val objective: Int, val time: Long, val instantiation: String){

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
