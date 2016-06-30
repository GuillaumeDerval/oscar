package oscar.cp.xcsp.examples

import oscar.algo.search.DefaultDFSearchListener
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.xcsp.modeling.DefaultConstraints
import oscar.cp.xcsp.XCSPSolver
import oscar.cp._

object Example2 extends App {
 
  val xcspSolver = new XCSPSolver with DefaultConstraints {
    override def allDifferent(vars: Iterable[CPIntVar]) : Constraint = oscar.cp.modeling.constraint.allDifferent(vars)
  }

  implicit val listener = DefaultDFSearchListener()

  val instance = """<?xml version="1.0" encoding="UTF-8"?>

<instance>
<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>

<domains nbDomains="1">
<domain name="D0" nbValues="3">0..2</domain>
</domains>

<variables nbVariables="3">
<variable name="V0" domain="D0"/>
<variable name="V1" domain="D0"/>
<variable name="V2" domain="D0"/>
</variables>

<constraints nbConstraints="1">
<constraint name="C1" arity="5" scope="V0 V1 V2 V3 V4" reference="global:allDifferent">
<parameters>
[ V0 V1 V2]
</parameters>
</constraint>
</constraints>



</instance>"""
  
  val (cp, vars) = xcspSolver.model(instance)
 
  cp.onSolution{
    vars.toSeq.sortBy(v=>v.name)foreach{v=>print(v.name + "=" + v.min+ " ")}
    println
  }
  cp.search(binaryFirstFail(vars.toSeq))
  println(cp.start())
 
}
