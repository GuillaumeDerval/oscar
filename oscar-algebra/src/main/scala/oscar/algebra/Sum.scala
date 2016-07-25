package oscar.algebra

/**
  * Created by smo on 20/07/16.
  */
class SumLoop[I,+T <: AnyType](val aLoop: ALoop[I,Expression[T]]) extends Term[T]{
  def eval(env: (Var) => Double): Double = ???

  override def value: Option[Double] = ???

  def toExpression = new Expression(Stream(new Prod(Const(1.0), Seq(this))))
}
