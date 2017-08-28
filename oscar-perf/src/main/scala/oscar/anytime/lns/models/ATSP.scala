package oscar.anytime.lns.models

import oscar.anytime.lns.Benchmark
import oscar.cp._

import scala.io.Source

/**
 * Asymmetric Traveling Salesman Problem
 */
class ATSP(val instance: String, val bestObj: Int = 0) extends CPModel with Benchmark {

  var lines = Source.fromFile(instance).getLines.toArray

  lines = lines.take(lines.size-1) // drop EOF
  val n = lines(3).split(":")(1).trim().toInt
  val dist = lines.drop(7).reduceLeft(_ + " " + _).split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)

  val distMatrixSucc = dist.sliding(n,n).toArray.map(_.toArray)

  val succ = Array.fill(n)(CPIntVar(0 until n))
  val obj = CPIntVar(0 until 1000000)
  add(minCircuit(succ, distMatrixSucc,obj),Strong)

  minimize(obj)

  override def decisionVariables: Array[CPIntVar] = succ

  override def problem: String = "ATSP"

  override def bestKnownObjective: Option[Int] = Some(bestObj)
}






















