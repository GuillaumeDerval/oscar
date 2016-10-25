package oscar.linprog

import org.scalatest.{FunSuite, Suite}
import oscar.algebra.{Linear, Model, SolverInterface}
import oscar.linprog.lpsolve.LPSolve

import scala.collection.immutable.IndexedSeq


abstract class LinearMathSolverTester extends FunSuite {

  private def trySolverAvailability(f: => Option[SolverInterface[Linear,Linear,Double]]): Option[SolverInterface[Linear,Linear,Double]] =
    try {
      f
    } catch {
      case _: UnsatisfiedLinkError => None
      case _: NoClassDefFoundError => None
      case t: Throwable => fail(t)
    }

  val lpsolve = trySolverAvailability {
    LPSolve.solve(new Model[Linear,Linear,Double]())
    Some(LPSolve)
  }

  override def nestedSuites: IndexedSeq[Suite] =
    IndexedSeq(
      testSuite(lpsolve, "LPSolve")
    )

  def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite
}