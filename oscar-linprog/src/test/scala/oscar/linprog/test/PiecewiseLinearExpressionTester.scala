package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import oscar.algebra._

@RunWith(classOf[JUnitRunner])
class PiecewiseLinearExpressionTests extends LinearMathSolverTester {
  override def testSuite(interface: Option[SolverInterface[_,Linear, Linear, Double]], solverName: String): FunSuite = {
    new PiecewiseLinearExpressionTester(interface)(solverName)
  }
}

class PiecewiseLinearExpressionTester(interface: Option[SolverInterface[_,Linear, Linear, Double]])(solverName: String) extends FunSuite with Matchers {

  override def suiteName: String = solverName + " - LPTester"

  implicit def i = interface.getOrElse{cancel()}

  def moreOrLess(d: Double) = d +- 1e-6

  ignore("Minimize |x|") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)

    model.withObjective(Minimize(abs(x)))

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0.0)
        solution(model.objective.expression) shouldBe moreOrLess(0.0)
    }
  }

  ignore("Minimize |-x|") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)

    minimize(abs(-x))

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0.0)
        solution(model.objective.expression) shouldBe moreOrLess(0.0)
    }
  }

  ignore("Maximize |x| in [-100; 50]") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 50)

    maximize(abs(x))

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(-100.0)
        solution(model.objective.expression) shouldBe moreOrLess(100.0)
    }
  }

  ignore("Maximize |x| in [-50; 100]") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -50, 100)

    maximize(abs(x))

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(100.0)
        solution(model.objective.expression) shouldBe moreOrLess(100.0)
    }
  }

  ignore("Minimize y s.t. y >= |x| with y, x in [-100; 100]") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)
    val y = VarNumerical("y", -100, 100)

    minimize(y)
    subjectTo("aboveAbsX" |: (y >= abs(x)))

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0.0)
        solution(y) shouldBe moreOrLess(0.0)

        solution(model.objective.expression) shouldBe moreOrLess(0.0)
    }
  }

  ignore("Remove abs expression") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)
    val y = VarNumerical("y", -100, 100)

    val absX = abs(x)

    minimize(y)
    subjectTo("aboveAbsX" |: y >= absX)


    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0.0)
        solution(y) shouldBe moreOrLess(0.0)

        solution(model.objective.expression) shouldBe moreOrLess(0.0)
    }

    // Remove expression and solve again
    //    solver.removeLinearConstraint("aboveAbsX")
    //    solver.removeAbsExpression(absX)

    model.solve match {
      case Optimal(solution) =>
        solution(y) shouldBe moreOrLess(-100.0)

        solution(model.objective.expression) shouldBe moreOrLess(-100.0)
    }
  }

  ignore("Minimize sign(x)") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)

    minimize(sign(x))

    model.solve match {
      case Optimal(solution) =>
        solution(x) should be < 0.0

        solution(model.objective.expression) shouldBe moreOrLess(-1.0)
    }
  }

  ignore("Maximize sign(x)") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)

    maximize(sign(x))

    model.solve match {
      case Optimal(solution) =>
        solution(x) should be > 0.0

        solution(model.objective.expression) shouldBe moreOrLess(1.0)
    }
  }

  ignore("Minimize |sign(x) - sign(y)| - x") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)
    val y = VarNumerical("y", -100, 100)

    minimize(abs(sign(x) - sign(y)) - x)

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(100.0)
        solution(y) should be > 0.0

        solution(model.objective.expression) shouldBe moreOrLess(-100.0)
    }
  }

  ignore("Maximize |sign(x) - sign(y)| + x") {
    implicit val model = new Model[Linear, Linear, Double]()
    val x = VarNumerical("x", -100, 100)
    val y = VarNumerical("y", -100, 100)

    maximize(abs(sign(x) - sign(y)) + x)

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(100.0)
        solution(y) should be < 0.0

        solution(model.objective.expression) shouldBe moreOrLess(102.0)
    }
  }
}
