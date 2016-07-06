package oscar.modeling.typetest

object Tests extends App {
  val expr1 = BinarySum(Constant(new Integer(3)), Constant(new Integer(2)))
  val expr2 = BinarySum(Constant(new Number(0.0)), Constant(new Integer(1)))
  val expr3 = BinarySum(expr1, expr2)
  val expr4 = expr1.mapSubExpressions(expr => BinarySum(expr, expr))

  println(expr1.evaluate())
  println(expr2.evaluate())
  println(expr3.evaluate())
  println(expr4.evaluate())

  println(expr1.evaluate().asInt)
  println(expr2.evaluate().asDouble)
  println(expr3.evaluate().asDouble)
  println(expr4.evaluate().asInt)
}
