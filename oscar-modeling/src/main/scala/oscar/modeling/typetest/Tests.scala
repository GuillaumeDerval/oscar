package oscar.modeling.typetest

object Tests extends App {
  val expr1 = BinarySum(Constant(3), Constant(2))
  val expr2 = BinarySum(Constant(0.0), Constant(1))
  val expr3 = BinarySum(expr1, expr2)
  val expr4 = expr1.mapSubExpressions(expr => BinarySum(expr, expr))
  val n = new Parameter[Integer]()
  val x = new Parameter[Number]()
  val expr5 = BinarySum(Constant(3), n)
  val expr6 = BinarySum(expr5, x)

  println(expr1.evaluate().asInt)
  println(expr2.evaluate().asDouble)
  println(expr3.evaluate().asDouble)
  println(expr4.evaluate().asInt)
  println(expr5.evaluate(new Data {
    def hasValue[T <: ValueType](v: Var[T]) = v == n
    def getValue[T <: ValueType](v: Var[T]) = {
      if (v == n) new Integer(2).asInstanceOf[T]
      else throw new UnsupportedOperationException()
    }
  }).asInt)
  println(expr6.evaluate(new Data {
    def hasValue[T <: ValueType](v: Var[T]) = v == n || v == x
    def getValue[T <: ValueType](v: Var[T]) = (
      if (v == n) new Integer(4)
      else if (v == x) new Number(1.5)
      else throw new UnsupportedOperationException()
      ).asInstanceOf[T]
  }).asDouble)
}
