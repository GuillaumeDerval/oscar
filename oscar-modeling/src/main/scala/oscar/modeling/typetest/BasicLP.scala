package oscar.modeling.typetest

object BasicLP extends App {
  val x0 = new DoubleVar("x1", 0, 40)
  val x1 = new DoubleVar("x2", 0, 1000)
  val x2 = new DoubleVar("x2", 0, 17)
  val x3 = new DoubleVar("x3", 2, 3)

  val eq1 = LinearEquality(new LinearExpression(Array(x1, x3), Array(Constant(1), Constant(-3.5))), Constant(0))
  val ineq1 = LinearLeq(new LinearExpression(Array(x0, x1, x2, x3), Array(Constant(-1), Constant(1), Constant(10), Constant(1))), Constant(20))
  val ineq2 = LinearLeq(new LinearExpression(Array(x0, x1, x2), Array(Constant(1), Constant(-3), Constant(1))), Constant(30))
  val obj = new LinearExpression(Array(x0, x1, x2, x3), Array(Constant(1), Constant(2), Constant(3), Constant(1)))

  val model = LinearModel(Array(eq1), Array(ineq1, ineq2), obj)

  val endStatus = LPInstantiate(model)

  for (x <- Seq(x0, x1, x2, x3))
    println(x.value)
}
