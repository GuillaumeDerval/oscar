package oscar.modeling.solvers.cp.searches.alns

import oscar.cp.searches.lns.CPIntSol

import scala.xml.{Elem, XML}

class XCSPIntSol(values: Array[Int], objective: Int, time: Long, val xcspInstantiation: String)
  extends CPIntSol(values, objective, time){

  override def asXml: Elem = {
    <solution>
      <objective>{objective}</objective>
      <time>{time}</time>
      {XML.loadString(xcspInstantiation)}
    </solution>
  }
}
