package oscar.algebra.linear

import org.scalatest.{Matchers, FunSuite}

class LinearExpressionTester extends FunSuite with Matchers {

  test("Sum of two linear expression") {
    val x = Var("x")
    val y = Var("y")

    val sum = x + y
  }
}
