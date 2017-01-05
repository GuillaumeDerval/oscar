package oscar.linprog

import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.{FunSuite, Matchers, Suite, Tag}
import oscar.algebra.{Linear, Model, SolverInterface}
import oscar.linprog.lpsolve.LPSolve

import scala.collection.immutable.IndexedSeq


abstract class LinearMathSolverTests extends FunSuite {

  private def canInstantiate(f: => Option[SolverInterface[Linear,Linear,Double]]): Option[SolverInterface[Linear,Linear,Double]] =
    try {
      f
    } catch {
      case _: UnsatisfiedLinkError => None
      case _: NoClassDefFoundError => None
      case t: Throwable => fail(t)
    }

  val lpsolve = canInstantiate {
    LPSolve.solve(new Model[Linear,Linear,Double]())
    Some(LPSolve)
  }

  override def nestedSuites: IndexedSeq[Suite] =
    IndexedSeq(
      testSuite(lpsolve, "LPSolve")
    )

  def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite
}

abstract class LinearMathSolverTester(interfaceOpt: Option[SolverInterface[Linear, Linear, Double]], val solverName: String) extends FunSuite with Matchers {

  implicit def solverInterface: SolverInterface[Linear, Linear, Double] = interfaceOpt.getOrElse( cancel() )

  def moreOrLess(d: Double): Spread[Double] = d +- 1e-6

  override def test(testName: String, testTags: Tag*)(testFun: => Unit): Unit = interfaceOpt match {
    case Some(i) => super.test(testName)(testFun)
    case None    => ignore(testName)(testFun)
  }
}