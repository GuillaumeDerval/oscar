package oscar.anytime.lns.models

import oscar.anytime.lns.Benchmark
import oscar.cp._

import scala.io.Source

/**
  * Traveling Salesman Problem
  *
  * Given a distance matrix between n cities,
  * find the shortest tour visiting each city exactly once.
  */
class TSP(val instance: String, val bestObj: Int = 0) extends CPModel with Benchmark {

  //DATA:
  val lines: Array[String] = Source.fromFile(instance).getLines.toArray
  val n: Int = lines(3).split(":")(1).trim().toInt
  val coords: Array[(Int, Int)] = parseCoordinates(lines)
  val distMatrix: Array[Array[Int]] = buildDistMatrix(coords)

  //Model:
  val succ: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until n))
  val obj = CPIntVar(0 until Int.MaxValue)
  add(minCircuit(succ, distMatrix, obj), Weak)

  // Search
  minimize(obj)

  override def decisionVariables: Array[CPIntVar] = succ

  override def problem: String = "TSP"

  override def bestKnownObjective: Option[Int] = Some(bestObj)

  //TSP Utils (from Oscar CP -> Examples -> util.reader -> TSPUtils):
  def parseCoordinates(data: Array[String]): Array[(Int, Int)] = {
    var lines = data.drop(6)
    val n = if(lines.last == "EOF") lines.length - 1 else lines.length
    val coordinates = Array.tabulate(n)(i => {
      val l = lines.head.trim.split("[ ,\t]+").map(_.toInt)
      val x = l(1)
      val y = l(2)
      lines = lines.drop(1)
      (x, y)
    })
    coordinates
  }

  def buildDistMatrix(coord: Array[(Int, Int)]): Array[Array[Int]] = {
    Array.tabulate(coord.length, coord.length)((i, j) => nint(getDist(coord(i), coord(j))).toInt)
  }

  def nint(x: Double): Int = {
    val i = math.floor(x).toInt
    val d = i + 0.5
    if (x > d) i + 1
    else if (x < d) i
    else if (i % 2 == 0) i
    else i + 1
  }

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy)
  }
}
