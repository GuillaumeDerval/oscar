
package oscar.algebra

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TermTests extends FunSuite with ShouldMatchers{

  test("Const"){

    Const(0) shouldBe Const(0.0)
    Const(1) shouldBe Const(1)

    Const(2.0).value shouldBe Some(2.0)

    def env(v: Var) = 200.0

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

    val model = new Model[Expression,Expression]()

    val v1 = model.Var0

    val vs = model.Var1(0 until 10)

    model.subjectTo(v1 + vs(5) + vs(7) <= 9)

    println(model)

    val N = 1000000000

    val manyManyVars = model.Var1(0 until N)
    val manyManyConstraints = new System[Expression]((0 until N).toStream.map(manyManyVars(_) <= 2))

    model.subjectTo(manyManyConstraints)

  }
}