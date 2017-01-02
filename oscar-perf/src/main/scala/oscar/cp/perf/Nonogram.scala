package oscar.cp.perf

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/


import oscar.cp._

import scala.xml.XML

/**
 * Object to solve a specific instance of the sets cw-m1c-ogd, cw-m1c-uk, rand-1 or rand-2 from :
 *      http://www.cril.univ-artois.fr/~lecoutre/benchmarks.html
 * Usage:
 *      scala BenchmarkTable instance_file.xml [nb_sols] algorithm
 * where algorithm is one of the following :
 *      CT for Compact Table
 *      CTS for Compact Table (implemented with sparse sets)
 *      STR2 for STR2
 *      STR3 for STR3
 *      GAC4 for GAC-4
 *      GAC4R for GAC-4R
 *      MDD4R for MDD-4R
 */
object Nonogram extends App {
  val TIME_OUT = 300
  
  val instance = "data/lecoutre/nonogram-gp-65.xml"
  val implem = "CT"
  val nbSolsMax = if (args.length > 2) args(1).toInt else 1
  
  import oscar.cp.constraints.tables.TableAlgo._
  import oscar.cp.constraints.tables._
  
  val implems = Map(
      "CT" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,CompactTable)),
      "STR2" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,STR2)),
      "STR3" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,STR3)),
      "GAC4" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,GAC4)),
      "GAC4R" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,GAC4R)),
      "MDD4R" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,MDD4R)),
      "AC5TCR" -> ((x: Array[CPIntVar], tuples: Array[Array[Int]]) => table(x, tuples,AC5TCRecomp))
  )
  
  implicit val solver: CPSolver = CPSolver()
  val xml = XML.loadFile(instance)

  /* Domains of the variables */
  val domains = (xml \ "domains" \ "domain").map { line => 
    val name = (line \ "@name").text
    val nbValues = (line \ "@nbValues").text.toInt
    val rangeBounds = line.text.split("\\.\\.").map(_.toInt)
    val range = rangeBounds(0) to rangeBounds(rangeBounds.length - 1)
    
    assert(range.length == nbValues)
    (name, range)
  }.toMap
  
  /* Variables */
  val varsArray = (xml \ "variables" \ "variable").map { line =>
    val name = (line \ "@name").text
    val domain = (line \ "@domain").text
    
    (name, CPIntVar(domains(domain), name))
  }
  val indexOfVar = varsArray.map(_._2).zipWithIndex.toMap
  val vars = varsArray.toMap
  
  /* Tuples */
  val relations = (xml \ "relations" \ "relation").map { line =>
    val name = (line \ "@name").text
    val nbTuples = (line \ "@nbTuples").text.toInt
    val tuples = line.text.split("\\|").map(tup => tup.split(" ").map(_.toInt))
    
    assert(nbTuples == tuples.length)
    (name, tuples)
  }.toMap
  
  var beforeInit = 0L
  var endInit = 0L
  var beforeSearch = 0L
  var finalTime = 0L
  try {
    /* Constraints */
    val constraints = (xml \ "constraints" \ "constraint").map { line =>
      val name = (line \ "@name").text
      val scopeName = (line \ "@scope").text.split(" ")
      val scope = scopeName.map(vars(_))
      val tab = relations((line \ "@reference").text)
      //println(tab.size+" x "+tab(0).size)
      //val cons = () => implems(implem)(scope, tab)
      (name, scope, tab)
    }
    
    /* We add the constraints */
    beforeInit = System.currentTimeMillis()
    val theConstraints = constraints.map(p => implems(implem)(p._2,p._3))
    //val theConstraints2 = constraints.map(p => implems("STR2")(p._2,p._3))
    
    add(theConstraints)

    
    endInit = System.currentTimeMillis()
    
    /* Search for a solution */
    val X = varsArray.map(_._2)


    search {
      binaryStatic(X, _.min)
    }
    
    beforeSearch = System.currentTimeMillis()
    val stats = start(nSols = nbSolsMax, timeLimit = TIME_OUT)
    finalTime = System.currentTimeMillis()

    val initTime = endInit - beforeInit
    val searchTime = finalTime - beforeSearch
    val totalTime = initTime + searchTime

    val time = stats.time
    if (time < TIME_OUT * 1000) {
      println(s"${instance}\t$initTime\t$searchTime\t$totalTime\t${stats.nFails-stats.nSols}\t${stats.nNodes}\t${stats.nSols}")
    
    }
    else {
      println(s"${instance}\t$initTime\tT.0.\tT.0.\tT.0.\tT.0.\tT.0.")
    }
  }
  catch {
    case e: OutOfMemoryError => println(s"$instance\tMemory Out")
    case e: oscar.cp.core.NoSolutionException => {
      val initTime = endInit - beforeInit
      val searchTime = finalTime - beforeSearch
      val totalTime = initTime + searchTime
      println(s"${instance.substring(42)}\tNo solution")
    }
    case e: Exception => throw e
  }
}