package oscar.linprog

import org.junit.runner.RunWith
import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import oscar.algebra._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


@RunWith(classOf[JUnitRunner])
class AbortTests extends LinearMathSolverTester{
  override def testSuite(interface: Option[SolverInterface[Linear,Linear,Double]], solverName: String): FunSuite = {
    new AbortTester(interface)(solverName)
  }
}

class AbortTester(interface: Option[SolverInterface[Linear, Linear, Double]])(solverName: String) extends FunSuite with Matchers {

  override def suiteName: String = solverName + " - AbortTester"

  implicit def i: SolverInterface[Linear, Linear, Double] = interface.getOrElse { cancel() }

  def moreOrLess(d: Double): Spread[Double] = d +- 1e-6

  test("Call to abort AFTER solve") {
    implicit val model = new Model[Linear, Linear, Double]()

    val n = 200

    val xs = Array.tabulate(n, n)((i, j) => VarBinary(s"x[$i,$j]"))
    val ys = Array.tabulate(n)(i => VarBinary(s"y$i"))

    val minSize = 10
    val maxSize = 100
    val sizes = Array.tabulate(n)(i => minSize + scala.util.Random.nextInt(maxSize - minSize))
    val binSize = n * maxSize / 2

    minimize(sumOf(ys.map(_.normalized)))

    model.subjectTo (
      new EquationSystem(for {
        i <- 0 until n
      } yield {
        s"allocation[$i]" |: (sum(0 until n)(j => xs(i)(j) * sizes(j).toDouble) <= ys(i) * binSize.toDouble)
      }))

    model.subjectTo (
      new EquationSystem(
      for {
        j <- 0 until n
      } yield {
        s"unicity[$j]" |: (sum(0 until n)(i => xs(i)(j)) === Const(1.0))
      }))

    val run = i.run(model)

    val startTime = System.currentTimeMillis()

    import scala.concurrent.ExecutionContext.Implicits.global
    val endStatusFuture = Future {
      run.solve
    }

    // wait
    Thread.sleep(500)

    // abort
    run.abort

    //get the results
    Await.result(endStatusFuture, 1 minute)

    val endTime = System.currentTimeMillis()

    (endTime - startTime).toDouble should be <= 1000.0
  }

  test("Call to abort BEFORE solve") {

    implicit val model = new Model[Linear, Linear, Double]()

    val x = VarNumerical("x", 100, 150)
    val y = VarNumerical("y", 80, 170)

    val o = x * 2.0 - y * 5.0
    minimize(o)

    model.subjectTo("E" |: x + y <= 200.0)

    val run = i.run(model)

    // Abort before solve should not prevent it
    run.abort

    run.solve match {
      case Optimal(solution) =>
        solution(x) shouldBe (100.0 +- 1e-6)
        solution(y) shouldBe (100.0 +- 1e-6)

        o.eval(solution) shouldBe (2 * 100 + -5 * 100.0 +- 1e-6)
      case r =>
        println(r)
        assert(false)
    }
  }
}
