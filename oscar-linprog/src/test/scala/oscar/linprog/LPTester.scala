package oscar.linprog

import org.junit.runner.RunWith
import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import oscar.algebra._


@RunWith(classOf[JUnitRunner])
class LPTests extends LinearMathSolverTests {
  override def testSuite(interface: Option[SolverInterface[Linear, Linear, Double]], solverName: String): FunSuite = {
    new LPTester(interface, solverName)
  }
}


class LPTester(interfaceOpt: Option[SolverInterface[Linear, Linear, Double]], solverName: String) extends LinearMathSolverTester(interfaceOpt, solverName) {

  override def suiteName: String = solverName + " - LPTester"

  test("Maximize objective under constraints") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 100, 150)
    val y = VarNumerical("y", 80, 170)

    maximize(-x * 2.0 + y * 5.0)

    subjectTo("E" |: x + y <= 200.0)

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(100)
        solution(y) shouldBe moreOrLess(100)

        model.objective.expression.eval(solution) shouldBe moreOrLess(-2 * 100 + 5 * 100)
    }
  }

  test("Minimize objective under constraints") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 100, 150)
    val y = VarNumerical("y", 80, 170)

    minimize(-2 * x + 5 * y)
    subjectTo("E" |: x + y >= 200.0)

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(150)
        solution(y) shouldBe moreOrLess(80)

        model.objective.expression.eval(solution) shouldBe moreOrLess(-2 * 150 + 5 * 80)
    }
  }

  test("subjectTo multiple constraints") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)

    maximize(x * 1.0 + y * 2.0 + z * 3.0)
    subjectTo("E" |: x + y <= 75.0)
    subjectTo("E" |: x + z <= 75.0)



    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(75.0)
        solution(z) shouldBe moreOrLess(75.0)

        model.objective.expression.eval(solution) shouldBe moreOrLess(1 * 0 + 2 * 75.0 + 3 * 75.0)
    }
  }

  test("Free variable") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", Double.MinValue, Double.MaxValue)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)

    maximize(x * 10.0 + y * 2.0 + z * 3.0)
    subjectTo("E" |: x + y <= 75.0)
    subjectTo("E" |: x + z <= 75.0)
    subjectTo("E" |: x >= 0)



    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(75.0)
        solution(y) shouldBe moreOrLess(0)
        solution(z) shouldBe moreOrLess(0)

        model.objective.expression.eval(solution) shouldBe moreOrLess(10 * 75.0 + 2 * 0 + 3 * 0)
    }
  }

  test("Detect infeasible problem") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 10)
    val y = VarNumerical("y", 80, 170)

    minimize(-x * 2.0 + y * 5.0)
    subjectTo("E" |: x + y >= 200.0)

    model.solve match {
      case Infeasible() =>
    }
  }

  test("Detect unbounded problem") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", Double.MinValue, Double.MaxValue)
    val y = VarNumerical("y", 80, 170)

    minimize(-x * 2.0 + y * 5.0)
    subjectTo("E" |: x + y >= 200.0)

    model.solve match {
      case Unbounded() =>
    }

  }

  test("Update variable bounds") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 100, 150)
    val y = VarNumerical("y", 80, 170)

    maximize(-x * 2.0 + y * 5.0)
    subjectTo("E" |: x + y <= 200.0)

    val run = solverInterface.run(model)

    run.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(100)
        solution(y) shouldBe moreOrLess(100)

        model.objective.expression.eval(solution) shouldBe moreOrLess(-2 * 100 + 5 * 100)
    }

    // Update bounds
    run.lowerBound(x) = 0
    run.upperBound(y) = 250

    run.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(200.0)

        model.objective.expression.eval(solution) shouldBe moreOrLess(-2 * 0 + 5 * 200.0)
    }

  }

  test("Update objective") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 100, 150)
    val y = VarNumerical("y", 80, 170)

    val obj = -x * 2.0 + y * 5.0

    maximize(obj)
    subjectTo("E" |: x + y <= 200.0)

    val run = solverInterface.run(model)

    run.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(100)
        solution(y) shouldBe moreOrLess(100)

        solution(obj) shouldBe moreOrLess(-2 * 100 + 5 * 100)
    }

    // Update objective
    run.setObjective(Minimize(obj))

    // Model has changed
    run.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(120)
        solution(y) shouldBe moreOrLess(80)

        solution(obj) shouldBe moreOrLess(-2 * 120 + 5 * 80)
    }
  }

  ignore("Remove constraint before solve") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)

    val o = x * 1.0 + y * 2.0 + z * 3.0
    maximize(o)
    subjectTo("cstr0" |: x + y <= 75.0)
    subjectTo("cstr1" |: x + z <= 75.0)

    // solver.removeLinearConstraint("cstr0")

    subjectTo("cstr0" |: x + y <= 60)



    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(60)
        solution(z) shouldBe moreOrLess(75.0)

        solution(o) shouldBe moreOrLess(1 * 0.0 + 2 * 60 + 3 * 75.0)
    }
  }

  ignore("Remove constraint after solve") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)

    val o = x * 1.0 + y * 2.0 + z * 3.0
    maximize(o)
    subjectTo("cstr0" |: x + y <= 75.0)
    subjectTo("cstr1" |: x + z <= 75.0)

    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(75.0)
        solution(z) shouldBe moreOrLess(75.0)

        solution(o) shouldBe moreOrLess(1 * 0.0 + 2 * 75.0 + 3 * 75.0)
    }
    // update model
    //solver.removeLinearConstraint("cstr0")
    subjectTo("cstr0" |: x + y <= 60)

    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(60)
        solution(z) shouldBe moreOrLess(75.0)

        solution(o) shouldBe moreOrLess(1 * 0.0 + 2 * 60 + 3 * 75.0)
    }
  }


  ignore("Remove unused variable before solve") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)
    val w = VarNumerical("w", 0, 100)

    maximize(x * 1.0 + y * 2.0 + z * 3.0)
    subjectTo("cstr0" |: x + y <= 75.0)
    subjectTo("cstr1" |: x + z <= 75.0)

    //    solver.removeVariable("w")

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(75.0)
        solution(z) shouldBe moreOrLess(75.0)

        solution(model.objective.expression) shouldBe moreOrLess(1 * 0.0 + 2 * 75.0 + 3 * 75.0)
    }
  }

  ignore("Remove unused variable after solve") {
    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 0, 100)
    val y = VarNumerical("y", 0, 100)
    val z = VarNumerical("z", 0, 100)
    val w = VarNumerical("w", 0, 100)

    maximize(x * 1.0 + y * 2.0 + z * 3.0)
    subjectTo("cstr0" |: x + y <= 75.0)
    subjectTo("cstr1" |: x + z <= 75.0)

    model.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(75.0)
        solution(z) shouldBe moreOrLess(75.0)

        solution(model.objective.expression) shouldBe moreOrLess(1 * 0.0 + 2 * 75.0 + 3 * 75.0)
    }

    //   solver.removeVariable("w")

    model.solve match {
      case Optimal(solution) =>

        solution(x) shouldBe moreOrLess(0)
        solution(y) shouldBe moreOrLess(75.0)
        solution(z) shouldBe moreOrLess(75.0)

        solution(model.objective.expression) shouldBe moreOrLess(1 * 0.0 + 2 * 75.0 + 3 * 75.0)
    }
  }

  ignore("Cannot remove used variable") {
    implicit val model = new Model[Linear, Linear, Double]()

    intercept[IllegalArgumentException] {
      val x = VarNumerical("x", 0, 100)
      val y = VarNumerical("y", 0, 100)
      val z = VarNumerical("z", 0, 100)

      maximize(x * 1.0 + y * 2.0 + z * 3.0)
      subjectTo("A" |: x + y <= 75.0)
      subjectTo("B" |: x + z <= 75.0)

      model.solve

      //solver.removeVariable("y")
    }
  }

  testForAllSolvers(MPSolverLib.lpSolvers, "Variable names are cropped if the solver used impose a restriction on their length") { implicit solver =>
    val x = MPFloatVar("veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVery" +
      "VeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVery" +
      "VeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVery" +
      "VeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVery" +
      "VeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVery" +
      "LongVariableName", 100, 150)
    val y = MPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(x + y <:= 200)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)
    solver.solutionQuality should equal(Success(Optimal))
  }
}
