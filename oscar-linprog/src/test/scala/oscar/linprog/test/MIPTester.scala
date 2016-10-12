package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import oscar.algebra._


@RunWith(classOf[JUnitRunner])
class MIPTests extends LinearMathSolverTester{
  override def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite = {
    new MIPTester(interface)(solverName)
  }
}


class MIPTester(interface: Option[SolverInterface[Linear, Linear, Double]])(solverName: String) extends FunSuite with Matchers {

  implicit def int2DoubleConst(i: Int) = Const(i.toDouble).normalized

  override def suiteName: String = solverName + " - MIPTester"

  implicit def i = interface.getOrElse{cancel()}

  def moreOrLess(d: Double) = d +- 1e-6

  test("Maximize objective under constraints with integer variables") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarInt("x", 0, 100)
    val y = VarNumerical("y", 0, 100)

    maximize(x * 100.0 + y)
    model.subjectTo("E" ||: "E" ||: x * 3.0 + y <= 14.5)

    model.solve match {
      case AOptimal(solution) =>
        solution(x) shouldBe moreOrLess(4.0)
        solution(y) shouldBe moreOrLess(14.5 - 3 * 4)
    }
  }

  test("Maximize objective under constraints with integer variables only") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarInt("x", 0, 100)
    val y = VarInt("y", 0, 100)

    maximize(x * 100.0 + y)
    subjectTo("E" ||: x * 3.0 + y <= 14.5)

    model.solve match {
      case AOptimal(solution) =>
        solution(x) shouldBe moreOrLess(4.0)
        solution(y) shouldBe moreOrLess(2.0)
    }
  }

  test("Change variable type from integer to continuous") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarInt("x", 0, 100)
    val y = VarNumerical("y", 0, 100)

    maximize(x * 100.0 + y)
    subjectTo("E" ||: x * 3.0 + y <= 14.5)

    val run = i.run(model)

    run.solve match {
      case AOptimal(solution) =>

        // Set x as a continuous var => performs the linear relaxation of the above problem
        run.setContinuous(x)

        run.solve match {
          case AOptimal(solution2) =>
            solution2(x) shouldBe moreOrLess(14.5 / 3.0)
            solution2(y) shouldBe moreOrLess(0.0)
        }
    }
  }

  test("Change variable type from continuous to integer") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarInt("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)

    maximize(x + y * 2.0 + z * 3.0)
    subjectTo("E" ||: x + y <= 75.5)
    subjectTo("E" ||: x + z <= 75.5)

    val run = i.run(model)

    run.solve match {
      case AOptimal(solution) =>

        run.setInteger(y)

        run.solve match {
          case AOptimal(solution2) =>
            solution2(x) shouldBe moreOrLess(0.0)
            solution2(y) shouldBe moreOrLess(75.0)
            solution2(z) shouldBe moreOrLess(75.5)
        }
    }
  }

  test("Maximize objective under constraints with binary variables") {
    implicit val model = new Model[Linear, Linear, Double]()


    val x = VarBinary("x")
    val y = VarNumerical("y", 0, 100)

    maximize(100 * x + 1 * y)
    subjectTo("E" ||: 3 * x + 1 * y <= 14.5)



    model.solve match {
      case AOptimal(solution) =>
        solution(x) shouldBe moreOrLess(1)
        solution(y) shouldBe moreOrLess(14.5 - 3 * 1)
    }
  }

  //  test("Retrieve gap value") {
  //    implicit val model = new Model[Linear,Linear,Double]()
  //
  //
  //    val y1 = MPBinaryVar("y1")
  //    val y2 = MPBinaryVar("y2")
  //
  //    val x1 = VarNumerical("x1", 0, 1)
  //    val x2 = VarNumerical("x2", 0, 1)
  //    val x3 = VarNumerical("x3", 0, 1)
  //    val x4 = VarNumerical("x4", 0, 1)
  //
  //    maximize(30*35*x3 + 10*25*x4 - 20*15*x1 - 20*20*x2)
  //    subjectTo( "E" ||: y1 === x1)
  //    subjectTo( "E" ||: y2 === x2)
  //    subjectTo( "E" ||: x1*20+x2*20 === x3*35+x4*25)
  //
  //    solver.subjectToGapCallback()
  //
  //
  //
  //    assert(solver.currentGap.get>=0)
  //
  //    solver.release()
  //  }
}
