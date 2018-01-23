package oscar.anytime.lns.models

import oscar.anytime.lns.Benchmark
import oscar.cp.{CPIntVar, CPSolver, Weak, circuit, element, minAssignment, post, sum}

class ProductMatrixTSP(val instance: String, val bestObj: Int = 0) extends  Benchmark{

  val NO_EDGE = 400
  val FAKE_WEIGHT = 100

  val (matrix, consumptions, costs) = readInstanceForCP(instance)

  val nVar = matrix.length
  val maxAssignment = nVar

  implicit val solver = CPSolver()
  solver.silent = false

  val x = Array.tabulate(nVar)(v => CPIntVar(0 to maxAssignment))

  val maxProduct = CPIntVar(0 until Int.MaxValue)
  solver.minimize(maxProduct)

  val pricesForVariable: Array[Array[Int]] = matrix

  val individualConsumptions = Array.tabulate(x.length)(i => element(pricesForVariable(i), x(i)))
  post(maxProduct === sum(individualConsumptions))


  post(circuit(x, false))
  post(minAssignment(x, pricesForVariable, maxProduct), Weak)

  def readInstanceForCP(fileURI: String): (Array[Array[Int]], Array[Int], Array[Int]) = {

    val lines: Array[String] = scala.io.Source.fromFile(fileURI).getLines().toArray

    val consumptions = lines(0).split(" ").map(_.toInt)
    val prices = lines(0).split(" ").map(_.toInt)

    val n = consumptions.length

    val fullMatrix = Array.tabulate(n,n)((i,j) => consumptions(i) * prices(j))

    (fullMatrix, consumptions, prices)
  }

  override def decisionVariables: Array[CPIntVar] = x

  override def bestKnownObjective: Option[Int] = Some(bestObj)

  override def problem: String = "ProdMatrixTSP"
}

