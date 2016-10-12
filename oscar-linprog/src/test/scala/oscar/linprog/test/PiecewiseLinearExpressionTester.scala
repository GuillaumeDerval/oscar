
/**
  * These tests have to be reenabled soon.
  */


//package oscar.linprog.test
//
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import org.scalatest.{FunSuite, Matchers}
//import oscar.algebra.{AOptimal, Const, Linear, Model, SolverInterface, VarNumerical}
//
//import scala.util.Success
//
//
//@RunWith(classOf[JUnitRunner])
//class AbortTests extends LinearMathSolverTester {
//  override def testSuite(interface: Option[SolverInterface[Linear, Linear, Double]], solverName: String): FunSuite = {
//    new PiecewiseLinearExpressionTester(interface)(solverName)
//  }
//}
//
//class PiecewiseLinearExpressionTester(interface: Option[SolverInterface[Linear, Linear, Double]])(solverName: String) extends FunSuite with Matchers {
//
//  implicit def int2DoubleConst(i: Int) = Const(i.toDouble).normalized
//
//  override def suiteName: String = solverName + " - LPTester"
//
//  implicit def i = interface.getOrElse{cancel()}
//
//  def moreOrLess(d: Double) = d +- 1e-6
//
//  test("Minimize |x|") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//
//    model.minimize(abs(x, -100, 100))
//
//    model.solve match {
//      case AOptimal(solution) =>
//        solution(x) shouldBe 0.0
//        solution(model.objective.expression) shouldBe moreOrLess(0.0)
//    }
//  }
//
//  test("Minimize |-x|") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//
//    minimize(abs(-x, -100, 100))
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(0.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Maximize |x| in [-100; 50]") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 50)
//
//    maximize(abs(x, -100, 50))
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(-100.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(100.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Maximize |x| in [-50; 100]") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -50, 100)
//
//    maximize(abs(x, -50, 100))
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(100.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(100.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Minimize y s.t. y >= |x| with y, x in [-100; 100]") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//    val y = VarNumerical("y", -100, 100)
//
//    minimize(y)
//    subjectTo(
//      "aboveAbsX" ||: (y >= abs(x, -100, 100))
//    )
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(0.0))
//    y.value should equalWithTolerance(Some(0.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Remove abs expression") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//    val y = VarNumerical("y", -100, 100)
//
//    val absX = solver.addAbsExpression(x, -100, 100)
//
//    minimize(y)
//    subjectTo(
//      "aboveAbsX" ||: (y >= solver.absLinearExpression(absX))
//    )
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(0.0))
//    y.value should equalWithTolerance(Some(0.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
//    solver.solutionQuality should equal(Success(Optimal))
//
//    // Remove expression and solve again
//    solver.removeLinearConstraint("aboveAbsX")
//    solver.removeAbsExpression(absX)
//
//    solver.solve should equal(SolutionFound)
//
//    y.value should equalWithTolerance(Some(-100.0))
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(-100.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Minimize sign(x)") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//
//    minimize(sign(x, -100, 100))
//
//    solver.solve should equal(SolutionFound)
//
//    x.value.get should be < 0.0
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(-1))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Maximize sign(x)") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//
//    maximize(sign(x, -100, 100))
//
//    solver.solve should equal(SolutionFound)
//
//    x.value.get should be > 0.0
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(1))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Minimize |sign(x) - sign(y)| - x") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//    val y = VarNumerical("y", -100, 100)
//
//    minimize(abs(sign(x, -100, 100) - sign(y, -100, 100), -100, 100) - x)
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(100))
//    y.value.get should be > 0.0
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(-100.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//
//  test("Maximize |sign(x) - sign(y)| + x") {
//    implicit val model = new Model[Linear, Linear, Double]()
//    val x = VarNumerical("x", -100, 100)
//    val y = VarNumerical("y", -100, 100)
//
//    maximize(abs(sign(x, -100, 100) - sign(y, -100, 100), -100, 100) + x)
//
//    solver.solve should equal(SolutionFound)
//
//    x.value should equalWithTolerance(Some(100))
//    y.value.get should be < 0.0
//
//    solver.objectiveValue.toOption should equalWithTolerance(Some(102.0))
//    solver.solutionQuality should equal(Success(Optimal))
//  }
//}
