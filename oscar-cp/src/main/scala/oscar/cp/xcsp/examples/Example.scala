package oscar.cp.xcsp.examples

import oscar.cp.core.Constraint
import java.io.File

import oscar.algo.search.DefaultDFSearchListener
import oscar.cp.core.variables.CPIntVar
import oscar.cp.xcsp.modeling.DefaultConstraints
import oscar.cp.xcsp.XCSPSolver
import oscar.cp._

object Example extends App {
 
  val xcspSolver = new XCSPSolver with DefaultConstraints {
    override def allDifferent(vars: Iterable[CPIntVar]) : Constraint = oscar.cp.modeling.constraint.allDifferent(vars)
  }

  implicit val listener = DefaultDFSearchListener()
  
  val (cp, vars) = xcspSolver.model(new File(args(0)))
 
  cp.onSolution{
    vars.toSeq.sortBy(v=>v.name)foreach{v=>print(v.name + "=" + v.min+ " ")}
    println
  }
  cp.search(binaryFirstFail(vars.toSeq))
  println(cp.start())
 
}