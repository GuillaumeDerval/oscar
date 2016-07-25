package oscar.algebra

import scala.collection.mutable.ArrayBuffer


class Equation[+T <: AnyType](val expr: Expression[T], val sense: ConstraintSense, val name: String) {
  require(name.nonEmpty)
  override def toString = s"$expr ${sense} 0"
  def ||:(s: String) = new Equation(expr, sense,s)
}

trait EquationDescription[+T <: AnyType]{
  def ||:(s: String): Equation[T]
}

class EQEquation[+T <: AnyType](expr: Expression[T]) extends EquationDescription[T]{
  def ||:(s: String) = new Equation(expr, EQ,s)
}
class LQEquation[+T <: AnyType](expr: Expression[T]) extends EquationDescription[T]{
  def ||:(s: String) = new Equation(expr, LQ,s)
}
class GQEquation[+T <: AnyType](expr: Expression[T])  extends EquationDescription[T]{
  def ||:(s: String) = new Equation(expr, GQ,s)
}

trait System[+T <: AnyType]{
  def equations: Stream[Equation[T]]
  def :||(s: String): System[T]
}


class StreamSystem[+T <: AnyType](val equations: Stream[Equation[T]], val name: String = "System") extends System[T]{

  def :||(s: String) = new StreamSystem(equations, s)

  override def toString = {
    val res = new StringBuffer()
    res.append(name)
    res.append("\n--------------------\n")
    equations.foreach{eq =>
      res.append(eq)
      res.append("\n")
    }
    res.append("\n--------------------\n")

    res.toString
  }
}

