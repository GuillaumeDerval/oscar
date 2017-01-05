
package oscar.algebra

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.math.Numeric.IntIsIntegral

class TermTests extends FunSuite with ShouldMatchers{

  test("Const"){

    Const(0) shouldBe Const(0)
    Const(1) shouldBe Const(1)

    def env(v: Var[_]) = 200.0

    Const(3.0).eval(env) shouldBe 3.0

    Const(2) shouldBe Const(2)

    Const(6) shouldNot be (Const(5))
  }

  test("Eval"){

    implicit val model = new Model[ExpressionDegree,ExpressionDegree,Int]()

    val c= Const(2)
    val x = Var0("x",0,3)
    val y = Var0("y",0,3)
    val z = Var0("z",0,3)

    def f(v: Var[Int]): Int = v.name match{
      case "x" => 2
      case "y" => 3
      case "z" => 0
    }

    f(x) shouldBe 2
    f(y) shouldBe 3

    (x + y).eval(f) shouldBe 5
    (x*y).eval(f) shouldBe 6
    (-x).eval(f) shouldBe -2
    (x*y*z).eval(f) shouldBe 0
    (x+z).eval(f) shouldBe 2

  }



  test("Degrees"){

    implicit val model = new Model[ExpressionDegree,ExpressionDegree,Int]()

    val c= Const(2)
    val x = Var0("x",0,3)
    val y = Var0("y",0,3)
    val z = Var0("z",0,3)

    val _0: NormalizedExpression[Constant,Int] = c+c+c

    val _10: NormalizedExpression[Constant,Int] = c*c


    val _1: NormalizedExpression[Linear,Int] = c+x+c
    val _2: NormalizedExpression[Linear,Int] = c + c*x

    val _21: NormalizedExpression[Linear,Int] = c + _2

    val _3: NormalizedExpression[Linear,Int] = x+y
    val _4: NormalizedExpression[Quadratic,Int] = x*y
    val _5: NormalizedExpression[Quadratic,Int] = x+y*z
    val _6: NormalizedExpression[ExpressionDegree,Int] = x*y*z + c
  }


}