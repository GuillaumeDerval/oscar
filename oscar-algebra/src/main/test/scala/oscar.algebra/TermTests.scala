
package oscar.algebra

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.math.Numeric.IntIsIntegral

class TermTests extends FunSuite with ShouldMatchers{

  test("Const"){

    Const(0) shouldBe Const(0.0)
    Const(1) shouldBe Const(1)

    Const(2.0).value shouldBe Some(2.0)

    def env(v: Var[_]) = 200.0

    Const(3.0).eval(env) shouldBe 3.0

    Const(2) shouldBe Const(2)

    Const(6) shouldNot be (Const(5))
  }
//
//  test("Prod"){
//    Prod(Stream(Const(2), Const(4),Const(6))).value shouldBe Some(48)
//
//    Prod(Stream(Const(2), Const(4),Const(6))) shouldBe Prod(Stream(Const(6), Const(2),Const(4)))
//
//  }
//
//  test("Size"){
//    val p = Prod((0 to 100000000).toStream.map(Const(_)))
//  }

  test("Model"){
//
//    implicit val model = new Model[AnyType,AnyType,Int]()
//
//    val vs = Var1(Indices("VS", 0 until 10), "X", Some(0), Some(20))
//
//    model.subjectTo( "E1" ||: Const(9) >= vs(5) + vs(7))
//
//    println(model)
//
//
//    val N = 1000000000
//
//    val manyManyVars = Var1[Int,Int](Indices("M",0 until N), "X")
//    val manyManyConstraints = new StreamSystem[Linear,Int]((0 until N).toStream.map("E2" ||: manyManyVars(_) <= Const(2)))
//
//    model.subjectTo(manyManyConstraints)

  }

  test("Degrees"){

    implicit val model = new Model[AnyType,AnyType,Int]()

    val c= Const(2)
    val x = Var0("x",0,3)
    val y = Var0("y",0,3)
    val z = Var0("z",0,3)

    val _0: Expression[Constant,Int] = c+c+c

    val _10: Expression[Constant,Int] = c*c


    val _1: Expression[Linear,Int] = c+x+c
    val _2: Expression[Linear,Int] = c + c*x

    val _21: Expression[Linear,Int] = c + _2

    val _3: Expression[Linear,Int] = x+y
    val _4: Expression[Quadratic,Int] = x*y
    val _5: Expression[Quadratic,Int] = x+y*z
    val _6: Expression[AnyType,Int] = x*y*z + c
  }


//  test("Indices"){
//    implicit val model = new Model[AnyType,AnyType,Int]()
//    val SetA = Indices("A", Vector(1,2,3))
//    val SetB = Indices("B", Vector("a","b","c"))
//    val vars = Var1(Indices("M",0 until 10),"X")
//
//    val SetC: ALoop[_,Equation[Linear,Int]] =
//      for(a <- SetA; b <- SetB) yield "E1" ||: Const[Int](2) >= vars(a)*Const[Int](2)
//
//    println(SetC)
//
//    println(SetC.toStream.toList)
//  }
}