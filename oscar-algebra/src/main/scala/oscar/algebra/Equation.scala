package oscar.algebra

import scala.collection.mutable.ArrayBuffer


class Equation[+T <: AnyType,+V](val expr: Expression[T,V], val sense: ConstraintSense, val name: String) {
  require(name.nonEmpty)
  override def toString = s"$expr ${sense} 0"
  def ||:(s: String) = new Equation(expr, sense,s)
}

trait EquationDescription[+T <: AnyType,+V]{
  def ||:(s: String): Equation[T,V]
}

class EQEquation[+T <: AnyType,+V](expr: Expression[T,V]) extends EquationDescription[T,V]{
  def ||:(s: String) = new Equation(expr, EQ,s)
}
class LQEquation[+T <: AnyType,+V](expr: Expression[T,V]) extends EquationDescription[T,V]{
  def ||:(s: String) = new Equation(expr, LQ,s)
}
class GQEquation[+T <: AnyType,+V](expr: Expression[T,V])  extends EquationDescription[T,V]{
  def ||:(s: String) = new Equation(expr, GQ,s)
}

trait System[+T <: AnyType,+V]{
  def equations: Stream[Equation[T,V]]
  def :||(s: String): System[T,V]
}


class StreamSystem[+T <: AnyType,+V](val equations: Stream[Equation[T,V]], val name: String = "System") extends System[T,V]{

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

