package oscar.cp.xcsp.examples

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.xcsp.modeling.DefaultConstraints
import oscar.cp._
import java.io.File
import oscar.cp.xcsp.XCSPSolver
import oscar.cp.constraints._
import oscar.cp.constraints.tables._
import scala.collection.mutable.Map

object STR2Bench extends App {

  val xcspSolver = new XCSPSolver with DefaultConstraints {
    // Scenario test for TableSTR2 + TableSTRNe
    override def table(x: Array[CPIntVar], tuples: Array[Array[Int]]): Constraint = new TableSTR2(x, tuples)
    override def tableNe(x: Array[CPIntVar], tuples: Array[Array[Int]]): Constraint = new TableSTRNe(x, tuples)
  }
  
  //args(0) = "/Users/ThanhKM/Documents/workspace/rand-6-40-5-40-15000-0.xml"
  println(args(0))
  val (cp, vars) = xcspSolver.model(new File(args(0)))

  cp.onSolution {
    vars.toSeq.sortBy(v => (v.name.length, v.name)) foreach { v => print(v.name + ":" + v.min + " ") }
    println
  }

  // Lexico ordering for variables
  //cp.search(binaryStatic(vars.toSeq.sortBy(v => (v.name.length, v.name))))
  
  /*
  // 0
  val valNames = "x[22] x[37] x[32] x[35] x[10] x[18] x[29] x[33] x[0] x[1] x[5] x[8] x[13] x[14] x[3] x[6] x[11] x[16] x[17] x[20] x[21] x[23] x[25] x[27] x[28] x[31] x[2] x[15] x[34] x[38] x[4] x[30] x[39] x[7] x[12] x[19] x[26] x[36] x[9] x[24]".split(" ")
  //val valNames = "x[8] x[36] x[15] x[28] x[29] x[34] x[38] x[6] x[17] x[19] x[20] x[24] x[25] x[31] x[37] x[2] x[3] x[14] x[16] x[18] x[35] x[0] x[7] x[9] x[10] x[12] x[21] x[22] x[30] x[32] x[33] x[39] x[11] x[23] x[26] x[27] x[1] x[5] x[13] x[4]".split(" ")
  //val valNames = "x[29] x[35] x[6] x[39] x[14] x[37] x[11] x[16] x[38] x[2] x[7] x[19] x[27] x[30] x[31] x[32] x[34] x[3] x[17] x[20] x[24] x[28] x[5] x[22] x[25] x[26] x[33] x[0] x[12] x[15] x[18] x[21] x[23] x[36] x[1] x[8] x[9] x[10] x[13] x[4]".split(" ")
  //val valNames = "x[2] x[14] x[19] x[26] x[33] x[6] x[35] x[38] x[10] x[16] x[17] x[27] x[29] x[34] x[0] x[3] x[9] x[20] x[21] x[24] x[25] x[28] x[30] x[31] x[32] x[1] x[4] x[8] x[11] x[13] x[37] x[7] x[15] x[22] x[36] x[39] x[5] x[12] x[18] x[23]".split(" ")
  //val valNames = "x[22] x[23] x[35] x[3] x[6] x[7] x[9] x[28] x[29] x[12] x[24] x[14] x[16] x[27] x[31] x[33] x[11] x[21] x[30] x[32] x[36] x[0] x[8] x[13] x[15] x[20] x[37] x[38] x[39] x[1] x[4] x[18] x[26] x[2] x[5] x[10] x[17] x[25] x[19] x[34]".split(" ")
  val dicts = valNames.zipWithIndex.toMap
  cp.search(binaryStatic(vars.toSeq.sortBy(v => dicts(v.name))))
  */
  
  // Dom
  cp.search(binaryFirstFail(vars.toSeq)) 
  
  // DomDeg
  //cp.search(binary(vars, x => - x.constraintDegree, _.min)) // 1000 * x.size
  
  print(cp.start(nSols = 1, timeLimit = 2000))
  println(cp.statistics)
}
