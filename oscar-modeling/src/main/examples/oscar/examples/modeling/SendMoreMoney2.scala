package oscar.examples.modeling

import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.SolverApp
import oscar.modeling.solvers.cp.{Branchings, SequentialCPSolving}
import oscar.modeling.solvers.mip.MIPSolving
import oscar.modeling.vars.IntVar

object SendMoreMoney2 extends SolverApp[String]() with SequentialCPSolving with MIPSolving {
  val all = Array.fill(8)(IntVar(0, 9))
  val Array(s, e, n, d, m, o, r, y) = all

  //SEND, MORE and MONEY must no start with a 0
  add(s > 0)
  add(m > 0)

  //They are AllDifferent
  add(AllDifferent(all))

  //And we have the main constraint
  add(        s*1000 + e*100 + n*10 + d +
              m*1000 + o*100 + r*10 + e ===
    m*10000 + o*1000 + n*100 + e*10 + y)

  //Add a CP search
  setCPSearch(Branchings.binaryStatic(all))

  onSolution {
    Array(s,e,n,d).map(_.min).mkString("")+"+"+
      Array(m,o,r,e).map(_.min).mkString("")+"="+
      Array(m,o,n,e,y).map(_.min).mkString("")+"\n"+
      Array(s,e,n,d,m,o,r,y).map(_.min).mkString(",")
  }

  println(solve().head)
}