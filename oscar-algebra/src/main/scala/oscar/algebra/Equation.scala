package oscar.algebra

import scala.collection.mutable.ArrayBuffer


class Equation[+T <: AnyType](expr: Expression[T], val sense: ConstraintSense) {
  override def toString = s"$expr ${sense} 0"
}

class EQEquation[+T <: AnyType](expr: Expression[T]) extends Equation(expr, EQ)
class LQEquation[+T <: AnyType](expr: Expression[T]) extends Equation(expr, LQ)
class GQEquation[+T <: AnyType](expr: Expression[T]) extends Equation(expr, GQ)



class System[+T <: AnyType](val equations: Stream[Equation[T]], name: String = "System"){
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

class Model[O <: AnyType, C <: AnyType]{

  var maxIndex = 0
  case class Var (id: Int) extends oscar.algebra.Var{
    def name = s"Var_$id"
  }
  def Var0 = {
    maxIndex += 1
    Var(maxIndex - 1)
  }
  case class Var1(range: Range){
    val id = maxIndex
    maxIndex += range.size
    def apply(i: Int) = {
      require(range.contains(i))
      Var(id+i)
    }
  }
  case class Var2(rangeA: Range, rangeB: Range){
    val id = maxIndex
    maxIndex += rangeA.size + rangeB.size
    def apply(i: Int, j: Int) = {
      require(rangeA.contains(i))
      require(rangeB.contains(j))
      Var(id+i*rangeB.size + j)
    }
  }
  case class Var3(rangeA: Range, rangeB: Range, rangeC: Range)



  val objectives = scala.collection.mutable.ListBuffer[O]()
  val constraints = scala.collection.mutable.ListBuffer[System[C]]()

  def subjectTo(eq: Equation[C]): Unit ={
    constraints += new System[C](Stream(eq))
  }

  def subjectTo(eqs: System[C]): Unit ={
    constraints += eqs
  }

  def withObjective(obj: O): Unit ={
    objectives += obj
  }

  override def toString = constraints.mkString("\n")
}
