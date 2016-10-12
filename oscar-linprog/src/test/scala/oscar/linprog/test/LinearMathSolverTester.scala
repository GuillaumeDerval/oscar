package oscar.linprog.test

import org.scalatest.{FunSuite, Suite}
import oscar.algebra.{Linear, Model, SolverInterface}
import oscar.linprog.LPSolve

import scala.collection.immutable.IndexedSeq


abstract class LinearMathSolverTester extends FunSuite{

  val lpsolve = try{
    LPSolve.solve(new Model[Linear,Linear,Double]())
    Some(LPSolve)
  }catch{
    case _ => None
  }


  override def nestedSuites: IndexedSeq[Suite] =
    IndexedSeq(
      testSuite(lpsolve, "LPSolve")
    )

  def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite
}